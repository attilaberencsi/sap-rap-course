/********** GENERATED on 11/16/2020 at 20:12:56 by CB0000000500**************/
 @OData.entitySet.name: 'Z_TRAVEL_AGENCY_ES5' 
 @OData.entityType.name: 'Z_TRAVEL_AGENCY_ES5Type' 
 define root abstract entity ZZ_TRAVEL_AGENCY_ES5501E042481 { 
 key AgencyId : abap.numc( 6 ) ; 
 @Odata.property.valueControl: 'Name_vc' 
 Name : abap.char( 31 ) ; 
 Name_vc : RAP_CP_ODATA_VALUE_CONTROL ; 
 @Odata.property.valueControl: 'Street_vc' 
 Street : abap.char( 30 ) ; 
 Street_vc : RAP_CP_ODATA_VALUE_CONTROL ; 
 @Odata.property.valueControl: 'PostalCode_vc' 
 PostalCode : abap.char( 10 ) ; 
 PostalCode_vc : RAP_CP_ODATA_VALUE_CONTROL ; 
 @Odata.property.valueControl: 'City_vc' 
 City : abap.char( 25 ) ; 
 City_vc : RAP_CP_ODATA_VALUE_CONTROL ; 
 @Odata.property.valueControl: 'Country_vc' 
 Country : abap.char( 3 ) ; 
 Country_vc : RAP_CP_ODATA_VALUE_CONTROL ; 
 @Odata.property.valueControl: 'PhoneNumber_vc' 
 PhoneNumber : abap.char( 30 ) ; 
 PhoneNumber_vc : RAP_CP_ODATA_VALUE_CONTROL ; 
 @Odata.property.valueControl: 'WebAddress_vc' 
 WebAddress : abap.char( 255 ) ; 
 WebAddress_vc : RAP_CP_ODATA_VALUE_CONTROL ; 
 
 } 
