CLASS zcl_hello_world_ab DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HELLO_WORLD_AB IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    out->write( |Hey Dude, nothing new so far, go further\n| ).
    out->write( sy-uname ).
    out->write( |\n{ cl_abap_context_info=>get_user_alias( ) }| ).
    out->write( |\n{ cl_abap_context_info=>get_system_url( ) }| ).
    out->write( |\n{ cl_abap_context_info=>get_user_business_partner_id( ) }| ).
    out->write( |\n{ cl_abap_context_info=>get_user_description( ) }| ).
    out->write( |\n{ cl_abap_context_info=>get_user_formatted_name( ) }| ).
    out->write( |\n{ cl_abap_context_info=>get_user_technical_name( ) }| ).
    out->write( |\n{ cl_abap_context_info=>get_user_time_zone( ) }| ).
    "out->write( |\n{ cl_abap_syst=>get_user_name( ) }| ).
    cl_abap_structdescr=>describe_by_name(
      EXPORTING
        p_name         = 'BAPILOGIND'
      RECEIVING
        p_descr_ref    = DATA(type_info)
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
*    DATA(logondata) = VALUE  bapilogond( ).
*    CALL FUNCTION 'bapi_user_get_detail'
*      EXPORTING
*        username  = sy-uname
*      IMPORTING
*        logondata = logondata.

  "data(bpu) = cl_bupa_bpu_api_factory=>get_instance( ).

  ENDMETHOD.
ENDCLASS.
