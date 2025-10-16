@AbapCatalog.sqlViewName: 'Z_CDS_LLM_AGENT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'LLM - CDS for fetching agent details'
@Metadata.ignorePropagatedAnnotations: true
define view ZCDS_LLM_AGENT
  as select from zllm_func_declar
    inner join   zllm_func_parame on  zllm_func_declar.app_id                 = zllm_func_parame.app_id
                                  and zllm_func_declar.business_function      = zllm_func_parame.business_function
                                  and zllm_func_declar.function_parameters_id = zllm_func_parame.parameters_id
                                  and zllm_func_declar.version                = zllm_func_parame.version
    left outer join zllm_ext_value on zllm_func_declar.function_name = zllm_ext_value.internal_value      
                                  and zllm_ext_value.type = 'FUNCTION_NAME'                             
{

  key zllm_func_declar.app_id,
  key zllm_func_declar.business_function,
  key zllm_func_declar.function_name,
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
      zllm_func_declar.type,
      zllm_func_declar.web_enabled

}
where zllm_func_declar.deletion_indicator = ''    //Ensure to leave out deleted agents
  and zllm_func_parame.deletion_indicator = ''    //Ensure to leave out parameters with deletion flag
