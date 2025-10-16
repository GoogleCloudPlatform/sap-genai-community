@AbapCatalog.sqlViewName: 'ZCDS_LLM_IASRPT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'LLM - CDS for building interaction report'
@Metadata.ignorePropagatedAnnotations: true
define view ZCDS_LLM_IA_SUMMARY_REPORT
//  with parameters
 //   p_app_id            : zllm_app_id,
 //   p_business_function : zllm_buss_function_name
  //p_date_from         : datum,
  //p_date_to           : datum
  as select distinct from zllm_log
//    left outer join zllm_star_rating on  zllm_star_rating.uuid_key = zllm_log.uuid_key

{
  key zllm_log.uuid_key,
  key zllm_log.app_id,
  key zllm_log.buss_function,

      max( case
           when zllm_log.object_type = 'ACTIONED'
           then 'Automation'
           when zllm_log.object_type = 'Freshdesk'
           then 'CS Hub'                                    //zllm_log.object_type
      //else 'No interaction'
           end )                          as interaction_type,

      max( case
           when zllm_log.object_type = 'ACTIONED'
           then zllm_log.object_key
           when zllm_log.object_type = 'Freshdesk'
           then zllm_log.object_key
           
      //else '-'
           end )                          as interaction_key,

      max( case
           when ( zllm_log.object_type = 'FUNCTION_CALL' )
           then zllm_log.object_key end ) as agent_used,


      zllm_log.user_name                  as interacted_by,

      max( case 
           when zllm_log.object_type = 'ACTIONED'
           then zllm_log.dat
           when zllm_log.object_type = 'Freshdesk'
           then zllm_log.dat
           when zllm_log.object_type = 'FUNCTION_CALL'
           then zllm_log.dat
           else '' end )                  as interaction_date,

      max( case 
           when zllm_log.object_type = 'ACTIONED'
           then zllm_log.time
           when zllm_log.object_type = 'Freshdesk'
           then zllm_log.time
           when zllm_log.object_type = 'FUNCTION_CALL'
           then zllm_log.time
           else '' end )                  as interaction_time,

      max( zllm_log.sentiment )           as sentiment,
      
      max( case
           when zllm_log.object_type = 'ACTIONED'
           then 'X'
      //else '-'
           end )                          as auto_replied

//      zllm_star_rating.rating             as star_rating

}

where
  //     zllm_log.app_id        = $parameters.p_app_id
 // and  zllm_log.buss_function = $parameters.p_business_function
  //and  zllm_log.dat           between $parameters.p_date_from and $parameters.p_date_to
 // and(
       zllm_log.object_type   = 'Freshdesk'
    or zllm_log.object_type   = 'FUNCTION_CALL'
    or zllm_log.object_type   = 'ACTIONED'
 // )
  and zllm_log.uuid_key         is not null
group by
  zllm_log.uuid_key,
  zllm_log.app_id,
  zllm_log.buss_function,
  zllm_log.user_name,
//  zllm_star_rating.rating,
  zllm_log.request_token_count,
  zllm_log.response_token_count
