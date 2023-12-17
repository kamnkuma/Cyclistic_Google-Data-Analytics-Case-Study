SELECT c.carId, c.brand, c.modelName, fm.fullModelName, c.bodyType, c.year, c.color, ft.fuelType, ft.isElectricVehicle, t.transmission, t.isManualTransmission,
    c.priceRubles, c.link, c.priceMileageRatio, ca.age, cp.priceUSD, r.regionId, r.listingId, r.location, e.engineId, e.engineName, e.vehicleConfiguration, es.mileage, es.engineDisplacement, 
    ce.fuelEfficiency, es.specId
FROM Car c
LEFT JOIN fuelType ft ON c.fuelType = ft.fuelType
LEFT JOIN FullModelName fm ON c.brand = fm.brand AND c.modelName = fm.modelName
LEFT JOIN transmissions t ON c.transmission = t.transmission
LEFT JOIN CarAge ca ON c.year = ca.year
LEFT JOIN CarPrices cp ON c.priceRubles = cp.priceRubles
LEFT JOIN Region r ON c.regId = r.regionId
LEFT JOIN Engine e ON c.carId = e.engineId
LEFT JOIN EngineSpecifications es ON c.carId = es.specId
LEFT JOIN CarEngineEfficiency ce ON es.mileage = ce.mileage AND es.engineDisplacement = ce.engineDisplacement

