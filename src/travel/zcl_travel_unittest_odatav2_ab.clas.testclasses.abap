"!@testing SRVB:ZAPI_TRAVEL_AB
CLASS ltc_CREATE DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: mo_client_proxy TYPE REF TO /iwbep/if_cp_client_proxy.

    METHODS: setup RAISING cx_static_check,
      create FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_CREATE IMPLEMENTATION.

  METHOD setup.
    " Create Proxy
    mo_client_proxy = cl_web_odata_client_factory=>create_v2_local_proxy( VALUE #( service_id      = 'ZAPI_TRAVEL_AB'
                                                                                     service_version = '0001' ) ).
  ENDMETHOD.

  METHOD create.
    DATA: ls_business_data TYPE ZC_Travel_AB,
          lo_request       TYPE REF TO /iwbep/if_cp_request_create,
          lo_response      TYPE REF TO /iwbep/if_cp_response_create.



    " Prepare business data
    "ls_business_data = value #(
    "                        traveluuid = '111122223333444455556666777788889999AAAA'
    "                        v_traveluuid = 'VTraveluuid'
    "                        travelid = '1'
    "                        v_travelid = 'VTravelid'
    "                        agencyid = '1'
    "                        v_agencyid = 'VAgencyid'
    "                        customerid = '1'
    "                        v_customerid = 'VCustomerid'
    "                        customername = 'Customername'
    "                        v_customername = 'VCustomername'
    "                        begindate = 20170101
    "                        v_begindate = 'VBegindate'
    "                        enddate = 20170101
    "                        v_enddate = 'VEnddate'
    "                        bookingfee = '1'
    "                        v_bookingfee = 'VBookingfee'
    "                        totalprice = '1'
    "                        v_totalprice = 'VTotalprice'
    "                        currencycode = 'Currencycode'
    "                        v_currencycode = 'VCurrencycode'
    "                        description = 'Description'
    "                        v_description = 'VDescription'
    "                        travelstatus = 'Travelstatus'
    "                        v_travelstatus = 'VTravelstatus'
    "                        lastchangedat = 20170101123000
    "                        v_lastchangedat = 'VLastchangedat'
    "                        locallastchangedat = 20170101123000
    "                        v_locallastchangedat = 'VLocallastchangedat'
    "                        hasdraftentity = abap_true
    "                        v_hasdraftentity = 'VHasdraftentity'
    "                        draftentitycreationdatetime = 20170101123000
    "                        v_draftentitycreationdatetime = 'VDraftentitycreationdatetime'
    "                        draftentitylastchangedatetime = 20170101123000
    "                        v_draftentitylastchang_4394027 = 'VDraftentitylastchang4394027'
    "                        hasactiveentity = abap_true
    "                        v_hasactiveentity = 'VHasactiveentity'
    "                        isactiveentity = abap_true
    "                        v_isactiveentity = 'VIsactiveentity'
    "                      ).


    " Navigate to the resource and create a request for the create operation
    lo_request = mo_client_proxy->create_resource_for_entity_set( 'Travel' )->create_request_for_create( ).

    " Set the business data for the created entity
    lo_request->set_business_data( ls_business_data ).

    " Execute the request
    lo_response = lo_request->execute( ).

    cl_abap_unit_assert=>fail( 'Implement your assertions' ).
  ENDMETHOD.

ENDCLASS.

"!@testing SRVB:ZAPI_TRAVEL_AB
CLASS ltc_READ_ENTITY DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: mo_client_proxy TYPE REF TO /iwbep/if_cp_client_proxy.

    METHODS: setup RAISING cx_static_check,
      read_entity FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_READ_ENTITY IMPLEMENTATION.

  METHOD setup.
    " Create Proxy
    mo_client_proxy = cl_web_odata_client_factory=>create_v2_local_proxy( VALUE #( service_id      = 'ZAPI_TRAVEL_AB'
                                                                                     service_version = '0001' ) ).
  ENDMETHOD.

  METHOD read_entity.
    DATA: ls_entity_key    TYPE ZC_Travel_AB-TravelUUID,
          ls_business_data TYPE ZC_Travel_AB,
          lo_resource      TYPE REF TO /iwbep/if_cp_resource_entity,
          lo_request       TYPE REF TO /iwbep/if_cp_request_read,
          lo_response      TYPE REF TO /iwbep/if_cp_response_read.



    " Set entity key
    "ls_entity_key = value #(
    "                    traveluuid = '111122223333444455556666777788889999AAAA'
    "                    isactiveentity = abap_true
    "                 ).


    " Navigate to the resource
    lo_resource = mo_client_proxy->create_resource_for_entity_set( 'Travel' )->navigate_with_key( ls_entity_key ).

    " Execute the request and retrieve the business data
    lo_response = lo_resource->create_request_for_read( )->execute( ).
    lo_response->get_business_data( IMPORTING es_business_data = ls_business_data ).

    cl_abap_unit_assert=>fail( 'Implement your assertions' ).
  ENDMETHOD.

ENDCLASS.

"!@testing SRVB:ZAPI_TRAVEL_AB
CLASS ltc_READ_LIST DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: mo_client_proxy TYPE REF TO /iwbep/if_cp_client_proxy.

    METHODS: setup RAISING cx_static_check,
      read_list FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_READ_LIST IMPLEMENTATION.

  METHOD setup.
    " Create Proxy
    mo_client_proxy = cl_web_odata_client_factory=>create_v2_local_proxy( VALUE #( service_id      = 'ZAPI_TRAVEL_AB'
                                                                                     service_version = '0001' ) ).
  ENDMETHOD.

  METHOD read_list.
    DATA: lt_business_data TYPE TABLE OF ZC_Travel_AB,
          lo_request       TYPE REF TO /iwbep/if_cp_request_read_list,
          lo_response      TYPE REF TO /iwbep/if_cp_response_read_lst.

    "DATA: lo_filter_factory   TYPE REF TO /iwbep/if_cp_filter_factory,
    "      lo_filter_node_1    TYPE REF TO /iwbep/if_cp_filter_node,
    "      lo_filter_node_2    TYPE REF TO /iwbep/if_cp_filter_node,
    "      lo_filter_node_root TYPE REF TO /iwbep/if_cp_filter_node,
    "      lt_range_traveluuid TYPE RANGE OF sysuuid_x16,
    "      lt_range_v_traveluuid TYPE RANGE OF sadl_gw_value_crtl_property.



    " Navigate to the resource and create a request for the read operation
    lo_request = mo_client_proxy->create_resource_for_entity_set( 'Travel' )->create_request_for_read( ).


    " Create the filter tree
    "lo_filter_factory = lo_request->create_filter_factory( ).
    "
    "lo_filter_node_1  = lo_filter_factory->create_by_range( iv_property_path     = 'traveluuid'
    "                                                        it_range             = lt_range_traveluuid ).
    "lo_filter_node_2  = lo_filter_factory->create_by_range( iv_property_path     = 'v_traveluuid'
    "                                                        it_range             = lt_range_v_traveluuid ).
    "lo_filter_node_root = lo_filter_node_1->and( lo_filter_node_2 ).
    "
    "lo_request->set_filter( lo_filter_node_root ).

    lo_request->set_top( 50 )->set_skip( 0 ).

    " Execute the request and retrieve the business data
    lo_response = lo_request->execute( ).
    lo_response->get_business_data( IMPORTING et_business_data = lt_business_data ).

    cl_abap_unit_assert=>fail( 'Implement your assertions' ).
  ENDMETHOD.

ENDCLASS.

"!@testing SRVB:ZAPI_TRAVEL_AB
CLASS ltc_UPDATE DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: mo_client_proxy TYPE REF TO /iwbep/if_cp_client_proxy.

    METHODS: setup RAISING cx_static_check,
      update FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_UPDATE IMPLEMENTATION.

  METHOD setup.
    " Create Proxy
    mo_client_proxy = cl_web_odata_client_factory=>create_v2_local_proxy( VALUE #( service_id      = 'ZAPI_TRAVEL_AB'
                                                                                     service_version = '0001' ) ).
  ENDMETHOD.

  METHOD update.
    DATA: ls_entity_key    TYPE ZC_Travel_AB-TravelUUID,
          ls_business_data TYPE ZC_Travel_AB,
          lo_resource      TYPE REF TO /iwbep/if_cp_resource_entity,
          lo_request       TYPE REF TO /iwbep/if_cp_request_update,
          lo_response      TYPE REF TO /iwbep/if_cp_response_update.



    " Set entity key
    "ls_entity_key = value #(
    "                    traveluuid = '111122223333444455556666777788889999AAAA'
    "                    isactiveentity = abap_true
    "                ).

    " Prepare the business data
    "ls_business_data = value #(
    "                        traveluuid = '111122223333444455556666777788889999AAAA'
    "                        v_traveluuid = 'VTraveluuid'
    "                        travelid = '1'
    "                        v_travelid = 'VTravelid'
    "                        agencyid = '1'
    "                        v_agencyid = 'VAgencyid'
    "                        customerid = '1'
    "                        v_customerid = 'VCustomerid'
    "                        customername = 'Customername'
    "                        v_customername = 'VCustomername'
    "                        begindate = 20170101
    "                        v_begindate = 'VBegindate'
    "                        enddate = 20170101
    "                        v_enddate = 'VEnddate'
    "                        bookingfee = '1'
    "                        v_bookingfee = 'VBookingfee'
    "                        totalprice = '1'
    "                        v_totalprice = 'VTotalprice'
    "                        currencycode = 'Currencycode'
    "                        v_currencycode = 'VCurrencycode'
    "                        description = 'Description'
    "                        v_description = 'VDescription'
    "                        travelstatus = 'Travelstatus'
    "                        v_travelstatus = 'VTravelstatus'
    "                        lastchangedat = 20170101123000
    "                        v_lastchangedat = 'VLastchangedat'
    "                        locallastchangedat = 20170101123000
    "                        v_locallastchangedat = 'VLocallastchangedat'
    "                        hasdraftentity = abap_true
    "                        v_hasdraftentity = 'VHasdraftentity'
    "                        draftentitycreationdatetime = 20170101123000
    "                        v_draftentitycreationdatetime = 'VDraftentitycreationdatetime'
    "                        draftentitylastchangedatetime = 20170101123000
    "                        v_draftentitylastchang_4394027 = 'VDraftentitylastchang4394027'
    "                        hasactiveentity = abap_true
    "                        v_hasactiveentity = 'VHasactiveentity'
    "                        isactiveentity = abap_true
    "                        v_isactiveentity = 'VIsactiveentity'
    "                      ).


    " Navigate to the resource and create a request for the update operation
    lo_resource = mo_client_proxy->create_resource_for_entity_set( 'Travel' )->navigate_with_key( ls_entity_key ).
    lo_request = lo_resource->create_request_for_update( /iwbep/if_cp_request_update=>gcs_update_semantic-put ).


    lo_request->set_business_data( ls_business_data ).

    " Execute the request and retrieve the business data
    lo_response = lo_request->execute( ).

    cl_abap_unit_assert=>fail( 'Implement your assertions' ).
  ENDMETHOD.

ENDCLASS.

"!@testing SRVB:ZAPI_TRAVEL_AB
CLASS ltc_DELETE_ENTITY DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: mo_client_proxy TYPE REF TO /iwbep/if_cp_client_proxy.

    METHODS: setup RAISING cx_static_check,
      delete_entity FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_DELETE_ENTITY IMPLEMENTATION.

  METHOD setup.
    " Create Proxy
    mo_client_proxy = cl_web_odata_client_factory=>create_v2_local_proxy( VALUE #( service_id      = 'ZAPI_TRAVEL_AB'
                                                                                     service_version = '0001' ) ).
  ENDMETHOD.

  METHOD delete_entity.
    DATA: ls_entity_key    TYPE ZC_Travel_AB-TravelUUID,
          ls_business_data TYPE ZC_Travel_AB,
          lo_resource      TYPE REF TO /iwbep/if_cp_resource_entity,
          lo_request       TYPE REF TO /iwbep/if_cp_request_delete.



    " Set entity key
    "ls_entity_key = value #(
    "                      traveluuid = '111122223333444455556666777788889999AAAA'
    "                      isactiveentity = abap_true
    "                  ).

    " Navigate to the resource and create a request for the delete operation
    lo_resource = mo_client_proxy->create_resource_for_entity_set( 'Travel' )->navigate_with_key( ls_entity_key ).
    lo_request = lo_resource->create_request_for_delete( ).


    " Execute the request
    lo_request->execute( ).

    cl_abap_unit_assert=>fail( 'Implement your assertions' ).
  ENDMETHOD.

ENDCLASS.
