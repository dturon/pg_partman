CREATE OR REPLACE FUNCTION check_name_length(p_object_name text, p_object_schema text DEFAULT NULL::text, p_suffix text DEFAULT NULL::text, p_table_partition boolean DEFAULT false, p_prefix text DEFAULT '__'::text) RETURNS text 
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
DECLARE
    v_new_length    int;
    v_new_name      text;
BEGIN

IF p_table_partition IS TRUE AND (p_suffix IS NULL OR p_object_schema IS NULL) THEN
    RAISE EXCEPTION 'Table partition name requires a schema and suffix value';
END IF;

IF p_table_partition THEN  -- 61 characters to account for _p in partition name
    IF char_length(p_object_name) + char_length(p_suffix) >= 61 THEN
        v_new_length := 61 - char_length(p_suffix);
        v_new_name := p_object_schema ||'.'|| substring(p_object_name from 1 for v_new_length) || '_p' || p_suffix; 
    ELSE
        v_new_name := p_object_schema ||'.'||p_object_name||'_p'||p_suffix;
    END IF;
ELSE
    IF p_suffix IS NOT NULL AND p_suffix = '_part_trig' THEN
        p_object_name := p_prefix||p_object_name;
    END IF; 

    IF char_length(p_object_name) + char_length(COALESCE(p_suffix, '')) >= 63 THEN
        v_new_length := 63 - char_length(COALESCE(p_suffix, ''));
        v_new_name := COALESCE(p_object_schema ||'.', '') || substring(p_object_name from 1 for v_new_length) || COALESCE(p_suffix, ''); 
    ELSE
        v_new_name := COALESCE(p_object_schema ||'.', '') || p_object_name||COALESCE(p_suffix, '');
    END IF;
END IF;

RETURN v_new_name;

END
$$;

DO $$
DECLARE
        _tgname text;
        _schema text;
        _parent_table text;
        _type text;
        _cmd text;
BEGIN
        FOR _tgname, _schema, _parent_table, _type 
        IN
            SELECT t.tgname, n.nspname, c.relname, pc.type
            FROM pg_trigger t
            JOIN pg_class c ON t.tgrelid = c.oid
            JOIN pg_namespace n ON c.relnamespace = n.oid
            JOIN @extschema@.part_config pc ON(n.nspname||'.'||c.relname = parent_table)
            WHERE t.tgname LIKE '%_part_trig' AND tgisinternal = false
        LOOP
            IF _type LIKE 'id%' THEN
                PERFORM @extschema@.create_function_id(_schema||'.'||_parent_table); 
            ELSIF _type LIKE 'time%' THEN
                PERFORM @extschema@.create_function_time(_schema||'.'||_parent_table);
            END IF;

            _cmd = format('DROP TRIGGER IF EXISTS %I ON %I.%I', _tgname, _schema, _parent_table);
            RAISE DEBUG '%', _cmd;
            EXECUTE _cmd;

            PERFORM @extschema@.create_trigger(_schema||'.'||_parent_table);

        END LOOP;


END$$;





