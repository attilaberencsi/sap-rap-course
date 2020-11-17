CLASS zcl_eml_ab DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_EML_AB IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA:
      op_switch TYPE c LENGTH 2 VALUE 'D1'.


    CASE op_switch.
      WHEN 'R1'."Read, only key is filled

        READ ENTITIES OF ZI_Travel_AB
          ENTITY Travel
            FROM VALUE #( ( TravelUUID = '12C133E6DD471EEB87BD373EA84AE24E' ) )
            RESULT DATA(Travels).

        out->write( Travels ).

      WHEN 'R2'.
        READ ENTITIES OF ZI_Travel_AB
          ENTITY Travel
            FIELDS ( AgencyID CustomerID )
            WITH VALUE #( ( TravelUUID = '12C133E6DD471EEB87BD373EA84AE24E' ) )
            RESULT Travels.

        out->write( Travels ).

      WHEN 'R3'.
        READ ENTITIES OF ZI_Travel_AB
          ENTITY Travel
            ALL FIELDS
            WITH VALUE #( ( TravelUUID = '12C133E6DD471EEB87BD373EA84AE24E' ) )
            RESULT Travels.

        out->write( Travels ).

      WHEN 'R4'.
        READ ENTITIES OF ZI_Travel_AB
          ENTITY Travel BY \_Booking
            ALL FIELDS
            WITH VALUE #( ( TravelUUID = '12C133E6DD471EEB87BD373EA84AE24E' ) )
            RESULT DATA(Bookings).

        out->write( Bookings ).

      WHEN 'R5'.
        READ ENTITIES OF ZI_Travel_AB
          ENTITY Travel
            ALL FIELDS
            WITH VALUE #( ( TravelUUID = '12c133e6-dd47-1eeb-87bd-373ea84ae24e' ) )
            RESULT Travels
            FAILED DATA(Failed)
            REPORTED DATA(Reported).


        out->write( Travels ).
        out->write( Failed ).
        out->write( Reported ).

      WHEN 'M1'.
        MODIFY ENTITIES OF ZI_Travel_AB
          ENTITY Travel
            UPDATE
              SET FIELDS WITH VALUE
                #( (  TravelUUID = '12C133E6DD471EEB87BD373EA84AE24E'
                      Description = 'Attilas 2nd RAP Dance Travel' ) )
        FAILED failed
        REPORTED Reported.

        COMMIT ENTITIES
          RESPONSE OF ZI_Travel_AB
          FAILED DATA(commit_failed)
          REPORTED DATA(commit_reported).

        out->write( 'Modify 1' ).

      WHEN 'C1'.
        MODIFY ENTITIES OF ZI_Travel_AB
          ENTITY Travel
            CREATE
              SET FIELDS WITH VALUE
                #( (  %cid = '00000001'
                      Description = 'Attilas 3rd RAP Dance Travel'
                      CustomerID = '14'
                      BeginDate = cl_abap_context_info=>get_system_date( )
                      EndDate = cl_abap_context_info=>get_system_date( ) + 20 ) )
        MAPPED DATA(mapped)
        FAILED failed
        REPORTED Reported.

        COMMIT ENTITIES
          RESPONSE OF ZI_Travel_AB
          FAILED commit_failed
          REPORTED commit_reported.

        out->write( 'Create 1' ).

      WHEN 'D1'.
        MODIFY ENTITIES OF ZI_Travel_AB
          ENTITY Travel
            DELETE FROM
            VALUE #( ( TravelUUID = '12C133E6DD471EEB87C39062BB2EA950' ) )
        FAILED failed
        REPORTED Reported.

        COMMIT ENTITIES
          RESPONSE OF ZI_Travel_AB
          FAILED commit_failed
          REPORTED commit_reported.

        out->write( 'Delete 1' ).

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
