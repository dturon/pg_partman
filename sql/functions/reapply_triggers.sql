/*
* Fuction to recreate triggers on all childs, can optionaly delete some triggers that not exist on parrent
*/
CREATE OR REPLACE FUNCTION reapply_triggers(p_parent_table text DEFAULT NULL, p_delete_not_exists boolean DEFAULT false)
    RETURNS VOID
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
DECLARE
    _parent_table text;
    _schema text;
    _child_table text;
    _tgname text;
    _cmd text;
BEGIN
        --recreate triggers for some/all parent partman tables
        FOR _parent_table, _child_table IN
            SELECT parent_table, ci.oid::pg_catalog.regclass 
            FROM pg_catalog.pg_namespace nc
            JOIN pg_catalog.pg_class c ON nc.oid = c.relnamespace
            JOIN @extschema@.part_config ON (nspname||'.'||relname = parent_table)
            JOIN pg_catalog.pg_inherits i ON(i.inhparent = c.oid)
            JOIN pg_catalog.pg_class ci ON (ci.oid=i.inhrelid)
            WHERE CASE WHEN p_parent_table IS NOT NULL THEN parent_table = p_parent_table ELSE True END
        LOOP
            PERFORM @extschema@.copy_triggers(_parent_table, _child_table, p_recreate := True);
        END LOOP;

        -- delete trigger that not exists on parent
        IF p_delete_not_exists THEN
            FOR _parent_table, _schema, _child_table, _tgname IN
                SELECT parent_table, nc.nspname, ci.relname, tri.tgname
                FROM pg_catalog.pg_namespace nc
                JOIN pg_catalog.pg_class c ON nc.oid = c.relnamespace
                JOIN @extschema@.part_config ON (nspname||'.'||relname = parent_table)
                JOIN pg_catalog.pg_inherits i ON(i.inhparent = c.oid)
                JOIN pg_catalog.pg_class ci ON (ci.oid=i.inhrelid)
                JOIN pg_catalog.pg_trigger tri ON (ci.relfilenode=tri.tgrelid)
                LEFT JOIN pg_catalog.pg_trigger tr ON (c.relfilenode=tr.tgrelid AND tr.tgname=tri.tgname)
                WHERE tr.tgname IS NULL 
                AND CASE WHEN p_parent_table IS NOT NULL THEN parent_table = p_parent_table ELSE True END
            LOOP
                _cmd = format('DROP TRIGGER IF EXISTS %I ON %I.%I', _tgname, _schema, _child_table);
                RAISE DEBUG '%', _cmd;
                EXECUTE _cmd;
            END LOOP;
        END IF;

END;
$$;
