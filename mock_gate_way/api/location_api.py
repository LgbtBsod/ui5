# api/location_api.py

@router.get("")
def get_locations(date: str, db: Session = Depends(get_db)):

    tree = LocationService.get_tree(db, date)

    return {
        "value": tree
    }