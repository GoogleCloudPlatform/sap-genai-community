@AbapCatalog.sqlViewName: 'ZCDS_LLM_IREPORT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'LLM - CDS for building interaction report'
@Metadata.ignorePropagatedAnnotations: true
define view ZCDS_LLM_INTERACTION_REPORT
  with parameters
    p_app_id            : zllm_app_id,
    p_business_function : zllm_buss_function_name
  //p_date_from         : datum,
  //p_date_to           : datum
  as select from    zllm_log
    left outer join zllm_star_rating on  zllm_star_rating.uuid_key = zllm_log.uuid_key
                                     and zllm_log.uuid_key         is not null
  //left outer join zllm_res_compare on zllm_res_compare.uuid_key = zllm_log.uuid_key

{
  key zllm_log.uuid_key,
  key zllm_log.app_id,
  key zllm_log.buss_function,

      max( case
           when zllm_log.object_type = 'Freshdesk'
           then zllm_log.object_type
      //else 'No interaction'
           end )                          as interaction_type,

      max( case
           when zllm_log.object_type = 'Freshdesk'
           then zllm_log.object_key
      //else '-'
           end )                          as interaction_key,

      max( case
           when ( zllm_log.object_type = 'FUNCTION_CALL' )
           then zllm_log.object_key end ) as agent_used,


      zllm_log.user_name                  as interacted_by,

      max( case when zllm_log.object_type = 'Freshdesk'
           then zllm_log.dat
           when zllm_log.object_type = 'FUNCTION_CALL'
           then zllm_log.dat
           else '' end )                  as interaction_date,

      max( case when zllm_log.object_type = 'Freshdesk'
           then zllm_log.time
           when zllm_log.object_type = 'FUNCTION_CALL'
           then zllm_log.time
           else '' end )                  as interaction_time,

      max( zllm_log.sentiment )           as sentiment,

      zllm_star_rating.rating             as star_rating

      //cast( zllm_log.request_token_count as abap.fltp ) * (0.0000000075) as request_token_cost_usd,
      //cast( zllm_log.response_token_count as abap.fltp ) * ( 0.0000003 ) as response_token_cost_usd
      //zllm_res_compare.edit_category      as ai_edit_category
      //zllm_res_compare.comparison_comments ) as ai_comparison_comments
}

where
       zllm_log.app_id        = $parameters.p_app_id
  and  zllm_log.buss_function = $parameters.p_business_function
  //and  zllm_log.dat           between $parameters.p_date_from and $parameters.p_date_to
  and(
       zllm_log.object_type   = 'Freshdesk'
    or zllm_log.object_type   = 'FUNCTION_CALL'
  )
group by
  zllm_log.uuid_key,
  zllm_log.app_id,
  zllm_log.buss_function,
  zllm_log.user_name,
  zllm_star_rating.rating,
  zllm_log.request_token_count,
  zllm_log.response_token_count
//zllm_res_compare.edit_category

union select from zllm_log
  left outer join zllm_star_rating on  zllm_star_rating.uuid_key   = zllm_log.uuid_key
                                   and zllm_star_rating.object_key = zllm_log.object_key
                                   and zllm_star_rating.user_name  = zllm_log.user_name
                                   and zllm_star_rating.dat        = zllm_log.dat
{

  key zllm_log.uuid_key,
  key zllm_log.app_id,
  key zllm_log.buss_function,

      max( case
           when zllm_log.object_type = 'Freshdesk'
           then zllm_log.object_type
      //else 'No interaction'
           end )                         as interaction_type,

      max( case
           when zllm_log.object_type = 'Freshdesk'
           then zllm_log.object_key
      //else '-'
           end )                         as interaction_key,

      max( case
           when ( zllm_log.object_type = 'FUNCTION_CALL' )
           then zllm_log.object_key
           else 'No agent details' end ) as agent_used,


      zllm_log.user_name                 as interacted_by,

      max( case when zllm_log.object_type = 'Freshdesk'
           then zllm_log.dat
           when zllm_log.object_type = 'FUNCTION_CALL'
           then zllm_log.dat
           else '' end )                 as interaction_date,

      max( case when zllm_log.object_type = 'Freshdesk'
           then zllm_log.time
           when zllm_log.object_type = 'FUNCTION_CALL'
           then zllm_log.time
           else '' end )                 as interaction_time,

      max( zllm_log.sentiment )          as sentiment,

      zllm_star_rating.rating            as star_rating


}

where
  zllm_log.uuid_key is null
group by
  zllm_log.uuid_key,
  zllm_log.app_id,
  zllm_log.buss_function,
  zllm_log.user_name,
  zllm_star_rating.rating,
  zllm_log.request_token_count,
  zllm_log.response_token_count
