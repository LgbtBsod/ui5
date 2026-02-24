from datetime import date

db.add(Person(
    perner=p["perner"],
    full_name=p["fullName"],
    position=p["position"],
    org_unit=p["orgUnit"],
    integration_name=p["integrationName"],
    begda=p.get("begda", date(2000, 1, 1)),
    endda=p.get("endda", None)
))