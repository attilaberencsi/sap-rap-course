CLASS zcl_sandkiste_ab DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SANDKISTE_AB IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA:
      c_op_switch TYPE c LENGTH 2 VALUE 'R2',
      Travels     TYPE TABLE FOR READ RESULT zi_travel_ab\\travel.

    CASE c_op_switch.
      WHEN 'R1'.
        READ ENTITIES OF ZI_Travel_AB
          ENTITY Travel
            "ALL FIELDS WITH VALUE #( ( TravelUUID = '12c133e6-dd47-1eeb-87bd-373ea84ae24e' ) )
            ALL FIELDS WITH VALUE #( ( TravelUUID = '12C133E6DD471EEB87BD373EA84AE24E' ) )
          RESULT DATA(TravelList)
          FAILED DATA(Failed)
          REPORTED DATA(Reported).
        out->write( 'RAP Tx. Read 1\n').
        out->write( TravelList ).

      WHEN 'R2'.
        READ ENTITIES OF ZI_Travel_AB
          ENTITY Travel
            ALL FIELDS WITH VALUE #( ( %control-TravelID = '12345678' ) )
          RESULT Travels
          FAILED Failed
          REPORTED Reported.

        out->write( 'RAP Tx. Read 2\n').
        LOOP AT failed-travel ASSIGNING FIELD-SYMBOL(<travel>).
          out->write( |{ <travel>-%fail-cause }\n| ).
        ENDLOOP.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
