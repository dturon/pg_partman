/*
* Function to apply cluster from parent to child table
*/
CREATE OR REPLACE FUNCTION apply_cluster(p_parent_table character varying, p_child_table character varying)
 RETURNS VOID
 LANGUAGE plpgsql SECURITY DEFINER
AS $$
DECLARE
        _idx_name text;
        _schema text;
        _parent_table text;
        _child_table text;
        _cmd text;
BEGIN

        SELECT n.nspname, c.relname as parent_table, ci.relname AS child_table, c2.relname idx_name 
        FROM pg_catalog.pg_class c                                                                                             
        JOIN pg_namespace n ON c.relnamespace = n.oid
        JOIN pg_catalog.pg_inherits inh ON(inh.inhparent = c.oid)
        JOIN pg_catalog.pg_class ci ON (ci.oid=inh.inhrelid), 
        pg_catalog.pg_class c2, 
        pg_catalog.pg_index i
        LEFT JOIN pg_catalog.pg_constraint con ON (conrelid = i.indrelid AND conindid = i.indexrelid AND contype IN ('p','u','x'))
        WHERE n.nspname||'.'||c.relname = p_parent_table
        AND n.nspname||'.'||ci.relname = p_child_table
        AND c.oid = i.indrelid AND i.indexrelid = c2.oid AND i.indisclustered
        INTO _schema, _parent_table, _child_table, _idx_name;
        
        IF NOT FOUND THEN
            RETURN;
        END IF;

        _cmd = format('ALTER TABLE %I.%I CLUSTER ON %I', _schema, _child_table, _child_table||substring(_idx_name FROM _parent_table||E'\(.*\)'));
        RAISE DEBUG '%', _cmd;
        EXECUTE _cmd;

END;
$$;
