@AbapCatalog.sqlViewName: 'Z_CDS_LLM_SAGENT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'LLM - CDS for fetching sub agent details'
@Metadata.ignorePropagatedAnnotations: true
define view ZCDS_LLM_SUB_AGENT
  with parameters
    p_supv_agent : zllm_function_name
  as select from    zllm_agent_map
    inner join      zllm_func_declar on  zllm_func_declar.function_name = zllm_agent_map.child
                                     and zllm_agent_map.type            = 'SUB'
                                     and zllm_func_declar.version       = zllm_agent_map.version
    inner join      zllm_func_parame on  zllm_func_declar.app_id                 = zllm_func_parame.app_id
                                     and zllm_func_declar.business_function      = zllm_func_parame.business_function
                                     and zllm_func_declar.function_parameters_id = zllm_func_parame.parameters_id
                                     and zllm_func_declar.version                = zllm_func_parame.version
    left outer join zllm_ext_value   on  zllm_func_declar.function_name = zllm_ext_value.internal_value
                                     and zllm_ext_value.type            = 'SUB_FUNCTION_NAME'
{

  key zllm_func_declar.app_id,
  key zllm_func_declar.business_function,
  key zllm_agent_map.child as function_name,
  key zllm_func_parame.parameters_id,
  key zllm_func_parame.parameter_name,
  key zllm_func_declar.version,
      zllm_func_declar.function_description,
      zllm_func_declar.function_parameters_id,
      zllm_func_declar.function_version,
      zllm_func_parame.parameter_description,
      zllm_func_parame.parameter_type,
      zllm_func_parame.is_required,
      zllm_ext_value.external_value,
      zllm_func_declar.automate,
      zllm_func_declar.type

}
where
      zllm_func_declar.deletion_indicator = '' //Ensure to leave out deleted agents
  and zllm_func_parame.deletion_indicator = ''
  and zllm_agent_map.parent      = $parameters.p_supv_agent
