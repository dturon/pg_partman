\set ON_ERROR_ROLLBACK 1
\set ON_ERROR_STOP true

BEGIN;
SELECT set_config('search_path','partman, public',false);

SELECT plan(20);

CREATE SCHEMA partman_test;

CREATE TABLE partman_test.test1_serial(a serial primary key,data int);
-- table for logging trigger events
CREATE TABLE partman_test.events(ev_id serial, ev_when text, ev_op text, ev_table text);


CREATE OR REPLACE FUNCTION partman_test.trg1() 
RETURNS trigger 
LANGUAGE plpgsql
AS $$
BEGIN
    RAISE DEBUG '% % %, data: %', TG_TABLE_NAME, TG_OP, TG_WHEN, NEW.data;
    INSERT INTO partman_test.events(ev_when, ev_op, ev_table) VALUES(TG_WHEN, TG_OP, TG_TABLE_NAME);
    RETURN NEW;
END;
$$;



CREATE OR REPLACE FUNCTION partman_test.trg2() 
RETURNS trigger 
LANGUAGE plpgsql
AS $$
BEGIN
    RAISE DEBUG '% % %, data: %', TG_TABLE_NAME, TG_OP, TG_WHEN, OLD.data;
    INSERT INTO partman_test.events(ev_when, ev_op, ev_table) VALUES(TG_WHEN, TG_OP, TG_TABLE_NAME);
    RETURN OLD;
END;
$$;

CREATE OR REPLACE FUNCTION partman_test.trg3() 
RETURNS trigger 
LANGUAGE plpgsql
AS $$
BEGIN
    RAISE DEBUG '% % %, data: %', TG_TABLE_NAME, TG_OP, TG_WHEN, NEW.data;
    INSERT INTO partman_test.events(ev_when, ev_op, ev_table) VALUES(TG_WHEN, TG_OP, TG_TABLE_NAME);
    -- trigger fail when fired on child
    IF TG_TABLE_NAME!='test1_serial' THEN
        RAISE EXCEPTION 'something wrong in trigger!';
    END IF;
    RETURN NEW;
END;
$$;


-- table triggers
CREATE TRIGGER trg_before_insert 
BEFORE INSERT
ON partman_test.test1_serial 
FOR EACH ROW 
EXECUTE PROCEDURE partman_test.trg1();


CREATE TRIGGER trg_after_insert 
AFTER INSERT
ON partman_test.test1_serial 
FOR EACH ROW 
EXECUTE PROCEDURE partman_test.trg1();

CREATE TRIGGER trg_before_update 
BEFORE UPDATE
ON partman_test.test1_serial 
FOR EACH ROW 
EXECUTE PROCEDURE partman_test.trg1();


CREATE TRIGGER trg_after_update 
AFTER UPDATE
ON partman_test.test1_serial 
FOR EACH ROW 
EXECUTE PROCEDURE partman_test.trg1();

CREATE TRIGGER trg_before_delete 
BEFORE DELETE
ON partman_test.test1_serial 
FOR EACH ROW 
EXECUTE PROCEDURE partman_test.trg2();


CREATE TRIGGER trg_after_delete 
AFTER DELETE
ON partman_test.test1_serial 
FOR EACH ROW 
EXECUTE PROCEDURE partman_test.trg2();




-- indexes
CREATE INDEX ON partman_test.test1_serial(data);



--partitions
SELECT * FROM create_parent(p_parent_table:='partman_test.test1_serial', p_control:='a', p_type:='id-static', p_interval:='100000', p_premake:=4, p_use_run_maintenance:=NULL::boolean, p_inherit_fk:=true, p_jobmon:=false, p_debug:=false);

--trigger tests
INSERT INTO partman_test.test1_serial SELECT 1, random()*1000;
SELECT results_eq(
    E'SELECT count(*)::int FROM partman_test.events WHERE ev_op=\'INSERT\' AND ev_when=\'BEFORE\';',
    ARRAY[1], 'Check count trigger on BEFORE INSERT events'
);

SELECT results_eq(
    E'SELECT ev_table FROM partman_test.events WHERE ev_op=\'INSERT\' AND ev_when=\'BEFORE\';',
    ARRAY['test1_serial_p0'], 'Check where fired trigger on BEFORE INSERT events'
);

SELECT results_eq(
    E'SELECT count(*)::int FROM partman_test.events WHERE ev_op=\'INSERT\' AND ev_when=\'AFTER\';',
    ARRAY[1], 'Check count trigger on AFTER INSERT events'
);

SELECT results_eq(
    E'SELECT ev_table FROM partman_test.events WHERE ev_op=\'INSERT\' AND ev_when=\'AFTER\';',
    ARRAY['test1_serial_p0'], 'Check where fired trigger on AFTER INSERT events'
);


UPDATE partman_test.test1_serial SET data=5 WHERE a=1;
SELECT results_eq(
    E'SELECT count(*)::int FROM partman_test.events WHERE ev_op=\'UPDATE\' AND ev_when=\'BEFORE\';',
    ARRAY[1], 'Check count trigger on BEFORE UPDATE events'
);

SELECT results_eq(
    E'SELECT ev_table FROM partman_test.events WHERE ev_op=\'UPDATE\' AND ev_when=\'BEFORE\';',
    ARRAY['test1_serial_p0'], 'Check where fired trigger on BEFORE UPDATE events'
);

SELECT results_eq(
    E'SELECT count(*)::int FROM partman_test.events WHERE ev_op=\'UPDATE\' AND ev_when=\'AFTER\';',
    ARRAY[1], 'Check count trigger on AFTER UPDATE events'
);

SELECT results_eq(
    E'SELECT ev_table FROM partman_test.events WHERE ev_op=\'UPDATE\' AND ev_when=\'AFTER\';',
    ARRAY['test1_serial_p0'], 'Check where fired trigger on AFTER UPDATE events'
);

DELETE FROM partman_test.test1_serial WHERE a=1;
SELECT results_eq(
    E'SELECT count(*)::int FROM partman_test.events WHERE ev_op=\'DELETE\' AND ev_when=\'BEFORE\';',
    ARRAY[1], 'Check count trigger on BEFORE DELETE events'
);

SELECT results_eq(
    E'SELECT ev_table FROM partman_test.events WHERE ev_op=\'DELETE\' AND ev_when=\'BEFORE\';',
    ARRAY['test1_serial_p0'], 'Check where fired trigger on BEFORE DELETE events'
);

SELECT results_eq(
    E'SELECT count(*)::int FROM partman_test.events WHERE ev_op=\'DELETE\' AND ev_when=\'AFTER\';',
    ARRAY[1], 'Check count trigger on AFTER DELETE events'
);

SELECT results_eq(
    E'SELECT ev_table FROM partman_test.events WHERE ev_op=\'DELETE\' AND ev_when=\'AFTER\';',
    ARRAY['test1_serial_p0'], 'Check where fired trigger on AFTER DELETE events'
);

--test move data
TRUNCATE partman_test.events;
INSERT INTO partman_test.test1_serial SELECT 10000000, random()*1000;

SELECT run_maintenance();
SELECT results_eq(
    'SELECT count(*)::int FROM ONLY partman_test.test1_serial;',
    ARRAY[1], 'Check count rows in parent'
);

--move data
SELECT partition_data_id('partman_test.test1_serial');

SELECT results_eq(
    'SELECT count(*)::int FROM ONLY partman_test.test1_serial;',
    ARRAY[0], 'Check count rows in parent'
);

SELECT results_eq(
    'SELECT count(*)::int FROM partman_test.test1_serial_p10000000;',
    ARRAY[1], 'Check count rows in child'
);

SELECT results_eq(
    E'SELECT count(*)::int FROM partman_test.events;',
    ARRAY[2], 'Check count events in event table'
);


--test bad trigger, that raise exception only on child table (we need dont lose data in this case!)
TRUNCATE partman_test.events;
TRUNCATE partman_test.test1_serial;

--drop other triggers on parrent 
DROP TRIGGER trg_before_insert ON partman_test.test1_serial;
DROP TRIGGER trg_after_insert ON partman_test.test1_serial;
DROP TRIGGER trg_before_update ON partman_test.test1_serial;
DROP TRIGGER trg_after_update ON partman_test.test1_serial;
DROP TRIGGER trg_before_delete ON partman_test.test1_serial;
DROP TRIGGER trg_after_delete ON partman_test.test1_serial;



CREATE TRIGGER trg_after_insert_working_only_on_parent 
AFTER INSERT
ON partman_test.test1_serial 
FOR EACH ROW 
EXECUTE PROCEDURE partman_test.trg3();

--refresh triggers on childs
SELECT reapply_triggers('partman_test.test1_serial', p_delete_not_exists:=True);

INSERT INTO partman_test.test1_serial SELECT 1, random()*1000;

SELECT results_eq(
    E'SELECT count(*)::int FROM partman_test.events WHERE ev_op=\'INSERT\' AND ev_when=\'AFTER\';',
    ARRAY[1], 'Check count trigger on AFTER INSERT events'
);

SELECT results_eq(
    E'SELECT ev_table FROM partman_test.events WHERE ev_op=\'INSERT\' AND ev_when=\'AFTER\';',
    ARRAY['test1_serial'], 'Check where fired trigger on AFTER INSERT events'
);

SELECT results_eq(
    'SELECT count(*)::int FROM ONLY partman_test.test1_serial;',
    ARRAY[1], 'Check count rows in parent'
);


SELECT undo_partition_id('partman_test.test1_serial', p_keep_table := false);


SELECT hasnt_table('partman_test', 'test1_serial_p0', 'Check test1_serial_p0 doesn''t exists anymore');

SELECT * FROM finish();
ROLLBACK;