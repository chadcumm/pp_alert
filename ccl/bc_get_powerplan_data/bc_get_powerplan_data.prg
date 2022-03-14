drop program bc_get_powerplan_data:dba go
create program bc_get_powerplan_data:dba
 
prompt
	"Output to File/Printer/MINE" = "MINE"   ;* Enter or select the printer or file name to send this report to.
	, "ORDER_ID" = 0
 
with OUTDEV, ORDER_ID
 
 
 
FREE RECORD reply
RECORD reply
(
%i cclsource:status_block.inc
)
 
free record plan_data
record plan_data
(
	1 plan_name = vc
	1 plan_group_nbr = f8
)
 
DECLARE cStatus 			= c1 WITH protect, noconstant("Z")
 
 
declare pw_group_nbr = f8 with protect,noconstant(0)
declare i=i4 with protect, noconstant(0)
declare dcOrderIdx = i4 with protect, noconstant(0)
 
 
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
	plan_data->plan_group_nbr = p.pw_group_nbr
	plan_data->plan_name = p.pw_group_desc
with nocounter
 
 
SET cStatus = "S"
 
#exit_script
SET reply->status_data->status = cStatus
 
set _memory_reply_string = cnvtrectojson(plan_data)
 
end
go
 
