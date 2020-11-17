@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Booking'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZI_Booking_AB_U
  as select from /dmo/booking

  association        to parent ZI_TRAVEL_AB_U as _Travel     on  $projection.TravelID = _Travel.TravelID
  association [1..1] to /DMO/I_Carrier        as _Carrier    on  $projection.CarrierID = _Carrier.AirlineID
  association [1..1] to /DMO/I_Customer       as _Customer   on  $projection.CustomerID = _Customer.CustomerID
  association [1..1] to /DMO/I_Connection     as _Connection on  $projection.CarrierID    = _Connection.AirlineID
                                                             and $projection.ConnectionID = _Connection.ConnectionID
  association [1..1] to /DMO/I_Flight         as _Flight     on  $projection.CarrierID    = _Flight.AirlineID
                                                             and $projection.ConnectionID = _Flight.ConnectionID
                                                             and $projection.FlightDate   = _Flight.FlightDate

{
  key travel_id     as TravelID,
  key booking_id    as BookingID,
      booking_date  as BookingDate,
      customer_id   as CustomerID,
      carrier_id    as CarrierID,
      connection_id as ConnectionID,
      flight_date   as FlightDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      flight_price  as FlightPrice,
      currency_code as CurrencyCode,



      /* associations */

      _Travel,
      _Carrier,
      _Customer,
      _Connection,
      _Flight

}
