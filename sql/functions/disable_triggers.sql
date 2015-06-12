/*
* Disable triggers on some/all partman parent and child tables (for reason moving data)
*/
CREATE OR REPLACE FUNCTION disable_triggers(p_parent_table text DEFAULT NULL, p_disable_triggers boolean DEFAULT True)
    RETURNS VOID
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
DECLARE
    _parent_table text;
    _prev_parent_table text;
    _schema text;
    _child_table text;
    _tgname text;
    _op_type text DEFAULT 'ENABLE';
    _cmd text;
    
BEGIN

    IF p_disable_triggers THEN
        _op_type = 'DISABLE';
    END IF;

    FOR _parent_table, _schema, _child_table, _tgname IN
        SELECT c.relname, nc.nspname, ci.relname
        FROM pg_catalog.pg_namespace nc
        JOIN pg_catalog.pg_class c ON nc.oid = c.relnamespace
        JOIN @extschema@.part_config ON (nspname||'.'||relname = parent_table)
        JOIN pg_catalog.pg_inherits i ON(i.inhparent = c.oid)
        JOIN pg_catalog.pg_class ci ON (ci.oid=i.inhrelid)
        WHERE CASE WHEN p_parent_table IS NOT NULL THEN parent_table = p_parent_table ELSE True END
    LOOP
        -- disable/enable triggers on parent
        IF _prev_parent_table IS DISTINCT FROM _parent_table THEN
            _prev_parent_table = _parent_table;
            _cmd = format('ALTER TABLE %I.%I %s TRIGGER ALL', _schema, _parent_table, _op_type);
            RAISE DEBUG '%', _cmd;
            EXECUTE _cmd;    
        END IF;
        -- disable/enable triggers on child
        _cmd = format('ALTER TABLE %I.%I %s TRIGGER ALL', _schema, _child_table, _op_type);
        RAISE DEBUG '%', _cmd;
        EXECUTE _cmd;
    END LOOP;

    -- parent dont have childs
    IF _prev_parent_table IS NULL THEN
        SELECT c.relname, nc.nspname
        FROM pg_catalog.pg_namespace nc
        JOIN pg_catalog.pg_class c ON nc.oid = c.relnamespace
        WHERE nspname||'.'||relname = p_parent_table
        INTO _parent_table, _schema;
        _cmd = format('ALTER TABLE %I.%I %s TRIGGER ALL', _schema, _parent_table, _op_type);
        RAISE DEBUG '%', _cmd;
        EXECUTE _cmd;
    END IF;

END;
$$;
