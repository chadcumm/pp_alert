drop program bc_powerplan_dc:dba go
create program bc_powerplan_dc:dba
 
prompt
	"Output to File/Printer/MINE" = "MINE"   ;* Enter or select the printer or file name to send this report to.
	, "ORDER_ID" = 0
 
with OUTDEV, ORDER_ID
 
 
 
FREE RECORD reply
RECORD reply
(
%i cclsource:status_block.inc
)
 
FREE RECORD global_criteria
RECORD global_criteria
(
  1 criteria[*]
  	2 encounter_type_flag = i2
  	2 time_qty = i4
  	2 time_unit_cd = f8
)
 
FREE RECORD exception_criteria
RECORD exception_criteria
(
  1	criteria[*]
  	2 encounter_type_flag = i2
    2 pathway_catalog_id = f8
    2 time_qty = i4
    2 time_unit_cd = f8
)
 
FREE RECORD discontinue
RECORD discontinue
(
  1 phases[*]
  	2 pw_group_nbr = f8
    2 pathway_id = f8
    2 encntr_id = f8
    2 updt_cnt = i4
  1 orders[*]
   2 order_id = f8
)
 
FREE RECORD sibling_load
RECORD sibling_load
(
  1 plans[*]
  	2 pw_group_nbr = f8
)
 
FREE RECORD outcome
RECORD outcome
(
  1 outcomes[*]
  	2 outcomeActId = f8
  	2 outcomeStatusCd = f8
  	2 updtCnt = i4
)
 
 
DECLARE cStatus 			= c1 WITH protect, noconstant("Z")
DECLARE cDischargeTypeMean	= c12 WITH constant("DISCHARGE"), protect
DECLARE cCarePlanTypeMean	= c12 WITH constant("CAREPLAN"), protect
DECLARE cPhaseTypeMean		= c12 WITH constant("PHASE"), protect
DECLARE cSubPhaseTypeMean	= c12 WITH constant("SUBPHASE"), protect
DECLARE cDOTTypeMean		= c12 WITH constant("DOT"), protect
DECLARE stat 				= i2 WITH noconstant(0), protect
DECLARE iCriteriaCnt 		= i4 WITH noconstant(0), protect
DECLARE iPhaseCnt	 		= i4 WITH noconstant(0), protect
DECLARE dPlannedStatusCd	= f8 WITH constant(uar_get_code_by("MEANING",16769,"PLANNED")), protect
DECLARE dFutureStatusCd 	= f8 WITH constant(uar_get_code_by("MEANING",16769,"FUTURE")), protect
DECLARE dInitiatedStatusCd 	= f8 WITH constant(uar_get_code_by("MEANING",16769,"INITIATED")), protect
DECLARE dInpatientEnctrCd 	= f8 WITH constant(uar_get_code_by("MEANING",69,"INPATIENT")), protect
DECLARE dDischargedStatusCd = f8 WITH constant(uar_get_code_by("MEANING",261,"DISCHARGED")), protect
DECLARE dHoursCd			= f8 WITH constant(uar_get_code_by("MEANING",340,"HOURS")), protect
DECLARE parameter_value		= VC
DECLARE	iMaxPhasesToDisc	= I4 WITH noconstant(0), protect
DECLARE iInptTimeQty		= I4 WITH noconstant(0), protect
DECLARE dInptTimeUnitCd		= f8 WITH noconstant(dHoursCd), protect
DECLARE iOtherTimeQty		= I4 WITH noconstant(0), protect
DECLARE dOtherTimeUnitCd	= f8 WITH noconstant(dHoursCd), protect
DECLARE iTimeQty			= I4 WITH noconstant(0), protect
DECLARE dTimeUnitCd			= f8 WITH noconstant(0.0), protect
DECLARE iIndex		 		= i4 WITH noconstant(0), protect
DECLARE start 				= i4 with protect, noconstant(0)
DECLARE cnt 				= i4 with protect, noconstant(0)
DECLARE iSiblingCnt	 		= i4 WITH noconstant(0), protect
DECLARE	dDate				= dq8
DECLARE	dCurrentDate		= dq8
DECLARE phase_loop_cnt		= i4 with protect, noconstant(0)
DECLARE sib_loop_cnt		= i4 with protect, noconstant(0)
DECLARE batch_size 			= i4 with protect, constant(20)
DECLARE exception_size		= i4 with protect, noconstant(0)
DECLARE phase_size 			= i4 with protect, noconstant(0)
DECLARE sibling_size		= i4 with protect, noconstant(0)
DECLARE REPORT_FAILURE (opName = vc, opStatus = c1, targetName = vc, targetValue = vc) = null
 
DECLARE dActivatedOutcomeCd		= f8 WITH constant(uar_get_code_by("MEANING", 30182, "ACTIVATED"))
DECLARE dFutureOutcomeCd		= f8 WITH constant(uar_get_code_by("MEANING", 30182, "FUTURE"))
DECLARE dDiscontinueOutcomeCd 	= f8 WITH constant(uar_get_code_by("MEANING", 30182, "DISCONTINUED"))
DECLARE dResOutcomeTypeCd		= f8 WITH constant(uar_get_code_by("MEANING", 16750, "RESULT OUTCO"))
DECLARE outcome_cnt				= i4 with protect, noconstant(0)
DECLARE outcome_size			= i4 with protect, noconstant(0)
DECLARE	outcome_add				= i4 with protect, noconstant(0)
 
declare pw_group_nbr = f8 with protect,noconstant(0)
declare i=i4 with protect, noconstant(0)
declare dcOrderIdx = i4 with protect, noconstant(0)
 
/***********************************************************************
	Get max phases to discontinue
***********************************************************************/
SET parameter_value = parameter(1,0)
IF (parameter_value = " ")
	SET iMaxPhasesToDisc = 500
ELSE
	SET iMaxPhasesToDisc = CNVTINT(parameter_value)
	IF (iMaxPhasesToDisc < batch_size)
		SET iMaxPhasesToDisc = batch_size
	ENDIF
ENDIF
 
 
select into "nl:"
plan_id = p.pw_group_nbr,
plan_name = p.pw_group_desc,
phase_id = p.pathway_id,
phase_name = p.description,
o.order_id,
o.order_mnemonic
from orders o, act_pw_comp apc, pathway p
plan o where o.order_id = $ORDER_ID
join apc where o.order_id = apc.parent_entity_id and apc.parent_entity_name = "ORDERS"
join p where apc.pathway_id = p.pathway_id
order by
plan_name,
plan_id,
phase_name,
phase_id,
o.order_mnemonic,
o.order_id
head plan_id
	pw_group_nbr = p.pw_group_nbr
with nocounter
 
/*********************************************************************************************************************************
	Find phases, careplans, and populate sibling list
*********************************************************************************************************************************/
SELECT INTO "nl:"
iEncounterTypeFlag = 1
FROM pathway p, encounter e
PLAN p
	WHERE (p.pw_group_nbr = pw_group_nbr)
JOIN e
	WHERE e.encntr_id = p.encntr_id ;AND e.encntr_status_cd = dDischargedStatusCd
	AND e.encntr_id != 0
ORDER BY iEncounterTypeFlag, p.pw_cat_group_id, p.pw_group_nbr, p.pathway_id
 
HEAD REPORT
 
	iPhaseCnt = 0
	iSiblingCnt = 0
	phase_loop_cnt = 1
 	sib_loop_cnt = 1
 	stat = alterlist(discontinue->phases, batch_size)
 	stat = alterlist(sibling_load->plans,  batch_size)
 	phase_size = batch_size
 	sibling_size = batch_size
 	dCurrentDate = CNVTDATETIME(curdate, curtime3)
 	dCurrentDate = CNVTDATETIMEUTC(dCurrentDate, 3)
 
 	HEAD iEncounterTypeFlag
 		dummyt = 0
 
 	HEAD p.pw_cat_group_id
 
 		iIndex = locatevalsort(iIndex, 1, exception_size, iEncounterTypeFlag,
			exception_criteria->criteria[iIndex]->encounter_type_flag, p.pw_cat_group_id,
			exception_criteria->criteria[iIndex]->pathway_catalog_id)
 
		IF (iIndex > 0)
			iTimeQty	= exception_criteria->criteria[iIndex]->time_qty
			dTimeUnitCd = exception_criteria->criteria[iIndex]->time_unit_cd
		ELSE
			IF (iEncounterTypeFlag = 1)
				iTimeQty	= iInptTimeQty
				dTimeUnitCd = dInptTimeUnitCd
			ELSE
				iTimeQty	= iOtherTimeQty
				dTimeUnitCd = dOtherTimeUnitCd
			ENDIF
		ENDIF
 
 		HEAD p.pw_group_nbr
 
 			IF (iMaxPhasesToDisc <= (iPhaseCnt + 1))
 				CALL cancel(1)
 			ENDIF
 
			DETAIL
 				IF (p.pathway_group_id <= 0.0 OR p.type_mean = cDOTTypeMean)
					IF (e.disch_dt_tm)
						dDate = CNVTDATETIMEUTC(e.disch_dt_tm, 3)
					ELSE
						dDate = dCurrentDate
					ENDIF
 
					dDate = CNVTLOOKAHEAD(BUILD(iTimeQty, ",H"), CNVTDATETIME(dDate))
 
	 				IF (CNVTDATETIME(dDate) <= CNVTDATETIME(dCurrentDate))
 
						iPhaseCnt = iPhaseCnt + 1
						IF (iPhaseCnt > phase_size)
							stat = alterlist(discontinue->phases, iPhaseCnt + (batch_size - 1))
							phase_size = iPhaseCnt + (batch_size - 1)
							phase_loop_cnt = phase_loop_cnt + 1
						ENDIF
 
						discontinue->phases[iPhaseCnt]->pathway_id 		= p.pathway_id
						discontinue->phases[iPhaseCnt]->pw_group_nbr 	= p.pw_group_nbr
						discontinue->phases[iPhaseCnt]->encntr_id 		= p.encntr_id
						discontinue->phases[iPhaseCnt]->updt_cnt 		= p.updt_cnt
 
						IF (p.cross_encntr_ind = 0 AND p.type_mean != cCarePlanTypeMean)
 
							iSiblingCnt = iSiblingCnt + 1
							IF (iSiblingCnt > sibling_size)
								stat = alterlist(sibling_load->plans, iSiblingCnt + (batch_size - 1))
								sibling_size = iSiblingCnt + (batch_size - 1)
								sib_loop_cnt = sib_loop_cnt + 1
							ENDIF
 
							sibling_load->plans[iSiblingCnt]->pw_group_nbr = p.pw_group_nbr
 
						ENDIF
					ENDIF
				ENDIF
 
WITH nocounter
 
 
/*********************************************************************************************************************************
	Get siblings to discontinue
*********************************************************************************************************************************/
IF (iSiblingCnt > 0)
 
	;Fill remaining spots in list with last valid value in list
	FOR (iIndex = (iSiblingCnt + 1) TO sibling_size)
		SET sibling_load->plans[iIndex]->pw_group_nbr = sibling_load->plans[iSiblingCnt]->pw_group_nbr
	ENDFOR
 
	SET iIndex = 0
	SET start = 1
 
	SELECT INTO "nl:"
	FROM (dummyt d1 with seq = value(sib_loop_cnt)), pathway p
	PLAN d1
		WHERE initarray(start, evaluate(d1.seq, 1, 1, start + batch_size))
	JOIN p
		WHERE expand(iIndex, start, start + (batch_size - 1), p.pw_group_nbr, sibling_load->plans[iIndex]->pw_group_nbr) AND
			(p.pw_status_cd = dPlannedStatusCd OR p.pw_status_cd = dFutureStatusCd) AND
			(p.type_mean = cPhaseTypeMean OR p.type_mean = cDOTTypeMean)
 
	DETAIL
 		IF (p.pathway_group_id <= 0.0 OR p.type_mean = cDOTTypeMean)
			iPhaseCnt = iPhaseCnt + 1
			IF (iPhaseCnt > phase_size)
				stat = alterlist(discontinue->phases, iPhaseCnt + (batch_size -1))
				phase_size = iPhaseCnt + (batch_size -1)
				phase_loop_cnt = phase_loop_cnt + 1
			ENDIF
 
			discontinue->phases[iPhaseCnt]->pathway_id 		= p.pathway_id
			discontinue->phases[iPhaseCnt]->pw_group_nbr 	= p.pw_group_nbr
			discontinue->phases[iPhaseCnt]->encntr_id 		= p.encntr_id
			discontinue->phases[iPhaseCnt]->updt_cnt 		= p.updt_cnt
		ENDIF
 
ENDIF
 
IF (iPhaseCnt <= 0)
	CALL REPORT_FAILURE("LOAD_PHASES TO DISCONTINUE","F","DCP_OPS_MAINT_PW_CLEANUP_EXPIRE",
       					"Unable to find and careplans and phases to discontinue")
    SET cStatus = "Z"
    GO TO exit_script
ENDIF
 
 
/***********************************************************************
	Fill remaining spots in list with last valid value in list
***********************************************************************/
FOR (iIndex = (iPhaseCnt + 1) TO phase_size)
	SET discontinue->phases[iIndex]->pathway_id 	= discontinue->phases[iPhaseCnt]->pathway_id
	SET discontinue->phases[iIndex]->pw_group_nbr 	= discontinue->phases[iPhaseCnt]->pw_group_nbr
	SET discontinue->phases[iIndex]->encntr_id 		= discontinue->phases[iPhaseCnt]->encntr_id
	SET discontinue->phases[iIndex]->updt_cnt 		= discontinue->phases[iPhaseCnt]->updt_cnt
ENDFOR
 
 
/*********************************************************************************************************************************
 	Get sub phases
*********************************************************************************************************************************/
SET iIndex = 0
SET start = 1
 
SELECT INTO "nl:"
FROM (dummyt d1 with seq = value(phase_loop_cnt))
		,pathway_reltn pr, pathway p
PLAN d1
	WHERE initarray(start, evaluate(d1.seq, 1, 1, start + batch_size))
JOIN pr
	WHERE expand(iIndex, start, start + (batch_size - 1), pr.pathway_s_id, discontinue->phases[iIndex]->pathway_id) AND
		pr.type_mean = cSubPhaseTypeMean
JOIN p
	WHERE p.pathway_id = pr.pathway_t_id AND
		(p.pw_status_cd = dPlannedStatusCd OR p.pw_status_cd = dFutureStatusCd OR p.pw_status_cd = dInitiatedStatusCd)
 
DETAIL
	iPhaseCnt = iPhaseCnt + 1
 
	IF (iPhaseCnt > phase_size)
		stat = alterlist(discontinue->phases, iPhaseCnt + (batch_size -1))
		phase_size = iPhaseCnt + (batch_size -1)
		phase_loop_cnt = phase_loop_cnt + 1
	ENDIF
 
	discontinue->phases[iPhaseCnt]->pathway_id 		= p.pathway_id
	discontinue->phases[iPhaseCnt]->pw_group_nbr	= p.pw_group_nbr
	discontinue->phases[iPhaseCnt]->encntr_id 		= p.encntr_id
	discontinue->phases[iPhaseCnt]->updt_cnt 		= p.updt_cnt
 
WITH nocounter
 
 
/***********************************************************************
	Fill remaining spots in list with last valid value in list
***********************************************************************/
FOR (iIndex = (iPhaseCnt + 1) TO phase_size)
	SET discontinue->phases[iIndex]->pathway_id 	= discontinue->phases[iPhaseCnt]->pathway_id
	SET discontinue->phases[iIndex]->pw_group_nbr 	= discontinue->phases[iPhaseCnt]->pw_group_nbr
	SET discontinue->phases[iIndex]->encntr_id 		= discontinue->phases[iPhaseCnt]->encntr_id
	SET discontinue->phases[iIndex]->updt_cnt 		= discontinue->phases[iPhaseCnt]->updt_cnt
ENDFOR
 
 
/*********************************************************************************************************************************
 	Get outcomes
*********************************************************************************************************************************/
SET stat = alterlist(outcome->outcomes, batch_size)
SET outcome_size = batch_size
 
SET iIndex = 0
SET start = 1
 
SELECT INTO "nl:"
FROM (dummyt d1 with seq = value(phase_loop_cnt))
		,act_pw_comp apc, outcome_activity oa
PLAN d1
	WHERE initarray(start, evaluate(d1.seq, 1, 1, start + batch_size))
JOIN apc
	WHERE expand(iIndex, start, start + (batch_size - 1), apc.pathway_id, discontinue->phases[iIndex]->pathway_id) AND
		apc.comp_type_cd = dResOutcomeTypeCd AND apc.parent_entity_name = "OUTCOME_ACTIVITY" AND apc.active_ind = 1 AND
		apc.parent_entity_id > 0
JOIN oa
	WHERE oa.outcome_activity_id = apc.parent_entity_id AND
		(oa.outcome_status_cd = dActivatedOutcomeCd OR oa.outcome_status_cd = dFutureOutcomeCd)
 
DETAIL
	IF (oa.end_dt_tm AND oa.outcome_status_cd = dActivatedOutcomeCd)
		dDate = CNVTDATETIMEUTC(oa.end_dt_tm, 3)
		IF (cnvtdatetime(dDate) > cnvtdatetime(dCurrentDate))
			outcome_add = 1
		ENDIF
	ELSE
		outcome_add = 1
	ENDIF
 
 	IF (outcome_add = 1)
 		outcome_cnt = outcome_cnt + 1
 
		IF (outcome_cnt > outcome_size)
			stat = alterlist(outcome->outcomes, outcome_cnt + (batch_size - 1))
			outcome_size = outcome_cnt + (batch_size - 1)
		ENDIF
 
		outcome->outcomes[outcome_cnt]->outcomeActId		= oa.outcome_activity_id
		outcome->outcomes[outcome_cnt]->outcomeStatusCd 	= dDiscontinueOutcomeCd
		outcome->outcomes[outcome_cnt]->updtCnt  			= oa.updt_cnt
 
		outcome_add = 0
	ENDIF
 
WITH nocounter
 
/*********************************************************************************************************************************
 	Get orders in phases
*********************************************************************************************************************************/
select into "nl:"
plan_id = p.pw_group_nbr,
plan_name = p.pw_group_desc,
phase_id = p.pathway_id,
phase_name = p.description,
o.order_id,
o.order_mnemonic,
o.order_status_cd
from
	pathway p
	,act_pw_comp apc
	,orders o
plan p
	where p.pw_group_nbr =  pw_group_nbr
join apc
	where apc.pathway_id = p.pathway_id
	and apc.parent_entity_name = "ORDERS"
join o
	where o.order_id = apc.parent_entity_id
order by
	o.order_id
head o.order_id
	i = (i+1)
	stat = alterlist(discontinue->orders,i)
	discontinue->orders[i].order_id = o.order_id
with nocounter,uar_code(d,1),format(date,";;q")
 
/*********************************************************************************************************************************
 	Resize the lists to actual size
*********************************************************************************************************************************/
SET stat = alterlist(discontinue->phases, iPhaseCnt)
SET phase_size = iPhaseCnt
 
SET stat = alterlist(outcome->outcomes, outcome_cnt)
SET outcome_size = outcome_cnt
 
call echorecord(discontinue)
call echorecord( outcome)
/*********************************************************************************************************************************
 	Call DCP.PwDiscontinuePhases to Discontinue Phases
*********************************************************************************************************************************/
DECLARE L_APPLICATION            = i4 WITH protect, noconstant(600005)
DECLARE L_TASK                   = i4 WITH protect, noconstant(601500)
DECLARE L_STEP	                = i4 WITH protect, noconstant(601504)
 
DECLARE hApplication             = i4 WITH protect, noconstant(0)
DECLARE hTask                    = i4 WITH protect, noconstant(0)
DECLARE hStep	                 = i4 WITH protect, noconstant(0)
DECLARE hItem					 = i4 WITH public, noconstant(0)
DECLARE hRequest				 = i4 WITH public, noconstant(0)
 
EXECUTE crmrtl
EXECUTE srvrtl
 
IF (uar_CrmBeginApp(L_APPLICATION, hApplication) != 0)
	CALL REPORT_FAILURE("CREATE_APPLICATION_HANDLE", "F", "DCP_OPS_PW_CLEANUP_DISCHARGE",
       					"Unable to create application handle when calling DCP.PwDiscontinuePhases")
    SET cStatus = "F"
ENDIF
 
IF (uar_CrmBeginTask(hApplication, L_TASK, hTask) != 0)
	CALL REPORT_FAILURE("CREATE_TASK_HANDLE", "F", "DCP_OPS_PW_CLEANUP_DISCHARGE",
       					"Unable to create task handle when calling DCP.PwDiscontinuePhases")
    SET cStatus = "F"
ENDIF
 
IF (uar_CrmBeginReq(hTask, "", L_STEP, hStep) != 0)
	CALL REPORT_FAILURE("CREATE_STEP_HANDLE", "F", "DCP_OPS_PW_CLEANUP_DISCHARGE",
       					"Unable to create step handle when calling DCP.PwDiscontinuePhases")
    SET cStatus = "F"
ENDIF
 
SET hRequest = uar_CrmGetRequest(hStep)
 
IF (hRequest <= 0)
	CALL REPORT_FAILURE("CREATE_REQUEST_HANDLE", "F", "DCP_OPS_PW_CLEANUP_DISCHARGE",
       					"Unable to create request handle when calling DCP.PwDiscontinuePhases")
	SET cStatus = "F"
ENDIF
 
FOR (iIndex = 1 TO iPhaseCnt)
	SET hItem = uar_SrvAddItem(hRequest, "phases")
 
	IF (hItem <= 0)
		CALL REPORT_FAILURE("CREATE_ITEM_HANDLE", "F", "DCP_OPS_PW_CLEANUP_DISCHARGE",
       					"Unable to create item handle for request when calling DCP.PwDiscontinuePhases")
       	SET iIndex = iPhaseCnt
	ELSE
		SET l_stat = uar_SrvSetDouble(	hItem, 	"pw_group_nbr",	discontinue->phases[iIndex]->pw_group_nbr)
		SET l_stat = uar_SrvSetDouble(	hItem, 	"pathway_id",   discontinue->phases[iIndex]->pathway_id)
		SET l_stat = uar_SrvSetDouble(	hItem, 	"encntr_id",    discontinue->phases[iIndex]->encntr_id)
		SET l_stat = uar_SrvSetLong  (	hItem, 	"updt_cnt",     discontinue->phases[iIndex]->updt_cnt)
	ENDIF
ENDFOR
 
IF (uar_CrmPerform(hStep) != 0)
	CALL REPORT_FAILURE("PERFORM REQUEST", "F", "DCP_OPS_PW_CLEANUP_DISCHARGE",
       					"Unable to perform request when calling DCP.PwDiscontinuePhases")
    SET cStatus = "F"
ENDIF
 
IF (hRequest > 0)
	CALL uar_CrmEndReq(hRequest)
ENDIF
 
IF (hTask > 0)
	CALL uar_CrmEndTask(hTask)
ENDIF
 
IF (hApplication > 0)
	CALL uar_CrmEndApp(hApplication)
ENDIF
 
 
/*********************************************************************************************************************************
 	Call dcp_s601520 to Discontinue Outcomes
*********************************************************************************************************************************/
SET L_APPLICATION            = 600005
SET L_TASK                   = 601520
SET L_STEP	                 = 601520
 
SET hApplication             = 0
SET hTask                    = 0
SET hStep	                 = 0
SET hItem					 = 0
SET hRequest				 = 0
 
IF (uar_CrmBeginApp(L_APPLICATION, hApplication) != 0)
	CALL REPORT_FAILURE("CREATE_APPLICATION_HANDLE", "F", "DCP_OPS_PW_CLEANUP_DISCHARGE",
       					"Unable to create application handle when calling dcp_s601520")
    SET cStatus = "F"
ENDIF
 
IF (uar_CrmBeginTask(hApplication, L_TASK, hTask) != 0)
	CALL REPORT_FAILURE("CREATE_TASK_HANDLE", "F", "DCP_OPS_PW_CLEANUP_DISCHARGE",
       					"Unable to create task handle when calling dcp_s601520")
    SET cStatus = "F"
ENDIF
 
IF (uar_CrmBeginReq(hTask, "", L_STEP, hStep) != 0)
	CALL REPORT_FAILURE("CREATE_STEP_HANDLE", "F", "DCP_OPS_PW_CLEANUP_DISCHARGE",
       					"Unable to create step handle when calling dcp_s601520")
    SET cStatus = "F"
ENDIF
 
SET hRequest = uar_CrmGetRequest(hStep)
 
IF (hRequest <= 0)
	CALL REPORT_FAILURE("CREATE_REQUEST_HANDLE", "F", "DCP_OPS_PW_CLEANUP_DISCHARGE",
       					"Unable to create request handle when calling dcp_s601520")
	SET cStatus = "F"
ENDIF
 
FOR (iIndex = 1 TO outcome_cnt)
	SET hItem = uar_SrvAddItem(hRequest, "outcomes")
 
	IF (hItem <= 0)
		CALL REPORT_FAILURE("CREATE_ITEM_HANDLE", "F", "DCP_OPS_PW_CLEANUP_DISCHARGE",
       					"Unable to create item handle for request when calling dcp_s601520")
       	SET iIndex = outcome_cnt
	ELSE
		SET l_stat = uar_SrvSetString(	hItem, 	"action",			"DISCHARGE")
		SET l_stat = uar_SrvSetDouble(	hItem, 	"outcomeActId",   	outcome->outcomes[iIndex]->outcomeActId)
		SET l_stat = uar_SrvSetDouble(	hItem, 	"outcomeStatusCd",  outcome->outcomes[iIndex]->outcomeStatusCd)
		SET l_stat = uar_SrvSetLong  (	hItem, 	"updtCnt",     		outcome->outcomes[iIndex]->updtCnt)
	ENDIF
ENDFOR
 
IF (uar_CrmPerform(hStep) != 0)
	CALL REPORT_FAILURE("PERFORM REQUEST", "F", "DCP_OPS_PW_CLEANUP_DISCHARGE",
       					"Unable to perform request when calling dcp_s601520")
    SET cStatus = "F"
ENDIF
 
IF (hRequest > 0)
	CALL uar_CrmEndReq(hRequest)
ENDIF
 
IF (hTask > 0)
	CALL uar_CrmEndTask(hTask)
ENDIF
 
IF (hApplication > 0)
	CALL uar_CrmEndApp(hApplication)
ENDIF
 
 
for (dcOrderIdx=1 to size(discontinue->orders,5))
	execute cst_eks_trigger_by_o "nl:","CST_EE_CANCEL_ORDER",value(discontinue->orders[dcOrderIdx].order_id)
endfor
 
/*********************************************************************************************************************************
 	BEGIN - Subroutine REPORT_FAILURE
*********************************************************************************************************************************/
SUBROUTINE REPORT_FAILURE(opName, opStatus, targetName, targetValue)
 
	/* get the current size of subeventstatus array */
	SET cnt = size(reply->status_data->subeventstatus,5)
 
	/*********************************************************************************
	*size of subevent is set to 1 in the .inc file, if the size is greater than one  *
	*then the script has already used the struct and the size needs to be incremented*
	*before use.  Same is true is the size is 1, but the first element is already    *
	*filled out.                                                                     *
	*********************************************************************************/
	if((cnt != 1) OR (cnt = 1 AND reply->status_data->subeventstatus[1]->operationstatus != NULL))
		/* increment the counter for future use */
		SET cnt = cnt + 1
		/* grow the size of subeventstatus array by one */
		SET stat = alter(reply->status_data->subeventstatus,value(cnt))
	endif
 
	/* populate the newly created element of subeventstatus array */
	SET reply->status_data->subeventstatus[cnt]->operationname = trim(opName)
	SET reply->status_data->subeventstatus[cnt]->operationstatus = trim(opStatus)
	SET reply->status_data->subeventstatus[cnt]->targetobjectname = trim(targetName)
	SET reply->status_data->subeventstatus[cnt]->targetobjectvalue = trim(targetValue)
END
/*********************************************************************************************************************************
	END - Subroutine REPORT_FAILURE
*********************************************************************************************************************************/
 
 
SET cStatus = "S"
 
#exit_script
SET reply->status_data->status = cStatus
 
set _memory_reply_string = cnvtrectojson(discontinue)
 
end
go
 
