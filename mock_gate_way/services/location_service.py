# services/location_service.py

from sqlalchemy import text
from sqlalchemy.orm import Session


class LocationService:

    @staticmethod
    def get_tree(db: Session, check_date):

        sql = text("""
        WITH RECURSIVE tree AS (

            SELECT
                l.node_id,
                l.parent_id,
                l.location_name,
                0 as hierarchy_level,
                1 as hierarchy_rank
            FROM locations l
            WHERE l.parent_id IS NULL
              AND l.begda <= :check_date
              AND (l.endda IS NULL OR l.endda >= :check_date)

            UNION ALL

            SELECT
                c.node_id,
                c.parent_id,
                c.location_name,
                p.hierarchy_level + 1,
                NULL
            FROM locations c
            JOIN tree p ON c.parent_id = p.node_id
            WHERE c.begda <= :check_date
              AND (c.endda IS NULL OR c.endda >= :check_date)
        )

        SELECT
            ROW_NUMBER() OVER () as hierarchy_rank,
            t.node_id,
            t.parent_id,
            t.location_name,
            t.hierarchy_level
        FROM tree t
        """)

        rows = db.execute(sql, {"check_date": check_date}).fetchall()

        # --- постобработка для parent_rank + tree_size ---

        output = []
        rank_map = {}

        for r in rows:
            rank_map[r.node_id] = r.hierarchy_rank

        for r in rows:
            parent_rank = rank_map.get(r.parent_id, 0)

            output.append({
                "hierarchy_rank": r.hierarchy_rank,
                "hierarchy_parent_rank": parent_rank,
                "hierarchy_tree_size": 1,  # упростим, можно считать детей
                "hierarchy_level": r.hierarchy_level,
                "node_id": r.node_id,
                "parent_id": r.parent_id,
                "location_name": r.location_name,
                "drill_state": "expanded"
            })

        return output