from sqlalchemy import text
from sqlalchemy.orm import Session


class HierarchyService:
    @staticmethod
    def get_tree(db: Session, check_date):
        sql = text(
            """
            WITH RECURSIVE tree AS (
                SELECT l.node_id, l.parent_id, l.location_name, 0 AS hierarchy_level
                FROM locations l
                WHERE l.parent_id IS NULL
                  AND l.begda <= :check_date
                  AND (l.endda IS NULL OR l.endda >= :check_date)
                UNION ALL
                SELECT c.node_id, c.parent_id, c.location_name, p.hierarchy_level + 1
                FROM locations c
                JOIN tree p ON c.parent_id = p.node_id
                WHERE c.begda <= :check_date
                  AND (c.endda IS NULL OR c.endda >= :check_date)
            )
            SELECT ROW_NUMBER() OVER () as hierarchy_rank, node_id, parent_id, location_name, hierarchy_level
            FROM tree
            """
        )
        rows = db.execute(sql, {"check_date": check_date}).fetchall()

        rank_map = {row.node_id: row.hierarchy_rank for row in rows}
        return [
            {
                "hierarchy_rank": row.hierarchy_rank,
                "hierarchy_parent_rank": rank_map.get(row.parent_id, 0),
                "hierarchy_tree_size": 1,
                "hierarchy_level": row.hierarchy_level,
                "node_id": row.node_id,
                "parent_id": row.parent_id,
                "location_name": row.location_name,
                "drill_state": "expanded",
            }
            for row in rows
        ]
