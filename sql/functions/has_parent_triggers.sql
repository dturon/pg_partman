/*
* Fuction returns true if parent table has other triggers then partition trigger
*/
CREATE OR REPLACE FUNCTION has_parent_triggers(p_parent_table text)
RETURNS BOOLEAN
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
  
BEGIN

    PERFORM 1 
    FROM pg_trigger t
    JOIN pg_class c ON t.tgrelid = c.oid
    JOIN pg_namespace n ON c.relnamespace = n.oid
    WHERE n.nspname||'.'||c.relname = p_parent_table 
    AND t.tgname NOT LIKE '%_part_trig'
    AND tgisinternal = false LIMIT 1;

    IF FOUND THEN
        RETURN True;
    END IF;

    RETURN False;

END;
$$;
