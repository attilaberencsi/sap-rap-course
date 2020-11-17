CLASS lhc_Booking DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE Booking.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE Booking.

    METHODS read FOR READ
      IMPORTING keys FOR READ Booking RESULT result.

    METHODS rba_Travel FOR READ
      IMPORTING keys_rba FOR READ Booking\_Travel FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lhc_Booking IMPLEMENTATION.

  METHOD delete.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD rba_Travel.
  ENDMETHOD.

ENDCLASS.
