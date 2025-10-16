CLASS zcx_llm DEFINITION
  PUBLIC
    INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .


    CONSTANTS:
      BEGIN OF fm_activation_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '078',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF fm_activation_failed.


    CONSTANTS:
      BEGIN OF transport_request_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '077',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF transport_request_failed.


    CONSTANTS:
      BEGIN OF abap_agent_create_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '076',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF abap_agent_create_failed.


    CONSTANTS:
      BEGIN OF prompt_properties_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '075',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF prompt_properties_failed.

    CONSTANTS:
      BEGIN OF prompt_connector_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '074',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF prompt_connector_failed.

    CONSTANTS:
      BEGIN OF region_not_found,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '073',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF region_not_found.

    CONSTANTS:
      BEGIN OF no_sub_agent,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '072',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF no_sub_agent.

    CONSTANTS:
      BEGIN OF article_not_found,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '071',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF article_not_found.

    CONSTANTS:
      BEGIN OF prompt_name_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '070',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF prompt_name_failed.

    CONSTANTS:
      BEGIN OF username_not_found,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '069',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF username_not_found.

    CONSTANTS:
      BEGIN OF email_verification_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '068',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF email_verification_failed.

    CONSTANTS:
      BEGIN OF perform_return_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '067',
        attr1 TYPE scx_attrname VALUE 'msg1',
        attr2 TYPE scx_attrname VALUE 'msg2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF perform_return_failed.

    CONSTANTS:
      BEGIN OF save_rag_class_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '066',
        attr1 TYPE scx_attrname VALUE 'msg1',
        attr2 TYPE scx_attrname VALUE 'msg2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF save_rag_class_failed.

    CONSTANTS:
      BEGIN OF dynamic_call_fail,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '065',
        attr1 TYPE scx_attrname VALUE 'msg1',
        attr2 TYPE scx_attrname VALUE 'msg2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF dynamic_call_fail.

    CONSTANTS:
      BEGIN OF prompt_chain_exists,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '064',
        attr1 TYPE scx_attrname VALUE 'msg1',
        attr2 TYPE scx_attrname VALUE 'msg2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF prompt_chain_exists.

    CONSTANTS:
      BEGIN OF save_agent_catalog_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '063',
        attr1 TYPE scx_attrname VALUE 'msg1',
        attr2 TYPE scx_attrname VALUE 'msg2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF save_agent_catalog_failed.

    CONSTANTS:
      BEGIN OF no_parent_function_provided,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '062',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF no_parent_function_provided.

    CONSTANTS:
      BEGIN OF function_module_does_not_exist,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '061',
        attr1 TYPE scx_attrname VALUE 'msg1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF function_module_does_not_exist.

    CONSTANTS:
      BEGIN OF save_rag_catalog_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '060',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF save_rag_catalog_failed.

    CONSTANTS:
      BEGIN OF save_agent_map_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '059',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF save_agent_map_failed.

    CONSTANTS:
      BEGIN OF save_gcs_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '058',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF save_gcs_failed.

    CONSTANTS:
      BEGIN OF no_chained_agents,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '057',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_chained_agents.

    CONSTANTS:
      BEGIN OF not_an_agent,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '056',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF not_an_agent.

    CONSTANTS:
      BEGIN OF lock_exists,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '055',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF lock_exists.

    CONSTANTS:
      BEGIN OF invalid_log_deletion_config,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '054',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_log_deletion_config.


    CONSTANTS:
      BEGIN OF simple_model_fail,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '053',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF simple_model_fail.

    CONSTANTS:
      BEGIN OF xlst_format_error,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '052',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF xlst_format_error.

    CONSTANTS:
      BEGIN OF save_sub_agent_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '047',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF save_sub_agent_failed.

    CONSTANTS:
      BEGIN OF save_conversation_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '047',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF save_conversation_failed.

    CONSTANTS:
      BEGIN OF log_note_save_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '046',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF log_note_save_failed.

    CONSTANTS:
      BEGIN OF user_filter_save_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '044',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF user_filter_save_failed.

    CONSTANTS:
      BEGIN OF no_fm_found,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '043',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_fm_found.

    CONSTANTS:
      BEGIN OF failed_to_update_prompt_eg,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '042',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF failed_to_update_prompt_eg.

    CONSTANTS:
      BEGIN OF failed_to_update_user_version,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '041',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF failed_to_update_user_version.

    CONSTANTS:
      BEGIN OF failed_to_update_user_role,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '040',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF failed_to_update_user_role.

    CONSTANTS:
      BEGIN OF failed_to_update_role,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '039',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF failed_to_update_role.

    CONSTANTS:
      BEGIN OF invalid_role_config,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '038',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_role_config.

    CONSTANTS:
      BEGIN OF role_already_exists,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '037',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF role_already_exists.

    CONSTANTS:
      BEGIN OF config_save_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '032',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF config_save_failed.

    CONSTANTS:
      BEGIN OF invalid_config_type,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '031',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_config_type.

    CONSTANTS:
      BEGIN OF version_control_record_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '030',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF version_control_record_failed.

    CONSTANTS:
      BEGIN OF failed_to_determine_version,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '029',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF failed_to_determine_version.

    CONSTANTS:
      BEGIN OF version_miss_match,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '028',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF version_miss_match.

    CONSTANTS:
      BEGIN OF version_update_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '027',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF version_update_failed.


    CONSTANTS:
      BEGIN OF invalid_version,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '026',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_version.

    CONSTANTS:
      BEGIN OF prompt_exists,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '025',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF prompt_exists.

    CONSTANTS:
      BEGIN OF func_call_exists,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '024',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF func_call_exists.

    CONSTANTS:
      BEGIN OF delete_prompt_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '023',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF delete_prompt_failed.

    CONSTANTS:
      BEGIN OF save_prompt_history_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '022',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF save_prompt_history_failed.

    CONSTANTS:
      BEGIN OF save_func_delcar_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '021',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF save_func_delcar_failed.

    CONSTANTS:
      BEGIN OF save_prompt_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '020',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF save_prompt_failed.

    CONSTANTS:
      BEGIN OF no_function_record,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '019',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_function_record.

    CONSTANTS:
      BEGIN OF no_prompt_record,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_prompt_record.

    CONSTANTS:
      BEGIN OF no_version_control_record,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_version_control_record.


    CONSTANTS:
      BEGIN OF version_interval_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF version_interval_failed.


    CONSTANTS:
      BEGIN OF no_number_range_interval,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_number_range_interval.

    CONSTANTS:
      BEGIN OF no_active_function_version,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_active_function_version.

    CONSTANTS:
      BEGIN OF no_active_app_version,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_active_app_version.

    CONSTANTS:
      BEGIN OF prompt_select_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF prompt_select_failed.

    CONSTANTS:
      BEGIN OF prompt_save_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF prompt_save_failed.

    CONSTANTS:
      BEGIN OF invalid_gcs_file,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_gcs_file.

    CONSTANTS:
      BEGIN OF invalid_inline_data,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_inline_data.


    CONSTANTS:
      BEGIN OF invalid_gcs_config,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_gcs_config.

    CONSTANTS:
      BEGIN OF invalid_gcs_bucket,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_gcs_bucket.

    CONSTANTS:
      BEGIN OF invalid_func_call_param,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_func_call_param.


    CONSTANTS:
      BEGIN OF invalid_app_id,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_app_id.


    CONSTANTS:
      BEGIN OF sdk_exception,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF sdk_exception.

    CONSTANTS:
      BEGIN OF no_function_call_properties,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_function_call_properties.

    CONSTANTS:
      BEGIN OF no_prompt,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_prompt.


    CONSTANTS:
      BEGIN OF gcs_search_error,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gcs_search_error.

    CONSTANTS:
      BEGIN OF no_function_declaration,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF no_function_declaration.

    CONSTANTS:
      BEGIN OF wrong_prefix_for_agent_name,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '033',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF wrong_prefix_for_agent_name.

    CONSTANTS:
      BEGIN OF external_values_not_maintained,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '034',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF external_values_not_maintained.

    CONSTANTS:
      BEGIN OF bot_creation_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '035',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF bot_creation_failed.

    CONSTANTS:
      BEGIN OF bot_not_found,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '036',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF bot_not_found.

    CONSTANTS:
      BEGIN OF odata_keys_not_provided,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '045',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF odata_keys_not_provided.

    CONSTANTS:
      BEGIN OF no_function_index,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '049',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF no_function_index.

    CONSTANTS:
      BEGIN OF save_func_index_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '050',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF save_func_index_failed.

    CONSTANTS:
      BEGIN OF save_ext_name_failed,
        msgid TYPE symsgid VALUE 'ZEX_LLM',
        msgno TYPE symsgno VALUE '051',
        attr1 TYPE scx_attrname VALUE 'MSG1',
        attr2 TYPE scx_attrname VALUE 'MSG2',
        attr3 TYPE scx_attrname VALUE 'MSG3',
        attr4 TYPE scx_attrname VALUE '',
      END OF save_ext_name_failed.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msg1     TYPE string OPTIONAL
        !msg2     TYPE string OPTIONAL
        !msg3     TYPE string OPTIONAL .



    DATA msg1 TYPE string .
    DATA msg2 TYPE string .
    DATA msg3 TYPE string .



  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_llm IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    CLEAR me->textid.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->msg1 = msg1.
    me->msg2 = msg2.
    me->msg3 = msg3.

  ENDMETHOD.



ENDCLASS.
