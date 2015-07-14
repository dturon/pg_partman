/*
* Fuction to reapply cluster on all childs
*/
CREATE OR REPLACE FUNCTION reapply_cluster(p_parent_table text DEFAULT NULL)
    RETURNS VOID
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
DECLARE
    _parent_table text;
    _child_table text;

BEGIN
        --reapply for some/all parent partman tables
        FOR _parent_table, _child_table IN
            SELECT parent_table, ci.oid::pg_catalog.regclass 
            FROM pg_catalog.pg_namespace nc
            JOIN pg_catalog.pg_class c ON nc.oid = c.relnamespace
            JOIN @extschema@.part_config ON (nspname||'.'||relname = parent_table)
            JOIN pg_catalog.pg_inherits i ON(i.inhparent = c.oid)
            JOIN pg_catalog.pg_class ci ON (ci.oid=i.inhrelid)
            WHERE CASE WHEN p_parent_table IS NOT NULL THEN parent_table = p_parent_table ELSE True END
        LOOP
            PERFORM @extschema@.apply_cluster(_parent_table, _child_table);
        END LOOP;
END;
$$;
