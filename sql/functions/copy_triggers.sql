/*
* Function to copy triggers from parent to child tables
*/
CREATE OR REPLACE FUNCTION copy_triggers(p_parent_table character varying, p_child_table character varying, p_recreate boolean DEFAULT false, p_disable_triggers boolean DEFAULT false)
 RETURNS VOID
 LANGUAGE plpgsql SECURITY DEFINER
 SET search_path TO public
AS $$
DECLARE
        _trigger text;
        _tgname text;
        _schema text;
        _parent_table text;
        _child_table text;
        _cmd text;
BEGIN
        FOR _trigger, _tgname, _schema, _parent_table, _child_table 
        IN
            SELECT replace(
                    pg_get_triggerdef(t.oid),
                    'ON '||p_parent_table,
                    'ON '||p_child_table
            ), t.tgname, n.nspname, c.relname, ci.relname

            FROM pg_trigger t
            JOIN pg_class c ON t.tgrelid = c.oid
            JOIN pg_namespace n ON c.relnamespace = n.oid
            JOIN pg_catalog.pg_inherits i ON(i.inhparent = c.oid)
            JOIN pg_catalog.pg_class ci ON (ci.oid=i.inhrelid)
            WHERE n.nspname||'.'||c.relname = p_parent_table 
            AND n.nspname||'.'||ci.relname = p_child_table
            AND t.tgname NOT LIKE '%_part_trig'
            AND tgisinternal = false
        LOOP
            IF p_recreate THEN
                _cmd = format('DROP TRIGGER IF EXISTS %I ON %I.%I', _tgname, _schema, _child_table);
                RAISE DEBUG '%', _cmd;
                EXECUTE _cmd;
            END IF;
            RAISE DEBUG '%', _trigger;
            EXECUTE _trigger;
        END LOOP;

        IF p_disable_triggers THEN
            _cmd = format('ALTER TABLE %I.%I DISABLE TRIGGER ALL', _schema, _child_table);
            RAISE DEBUG '%', _cmd;
            EXECUTE _cmd;
        END IF;
END;
$$;
