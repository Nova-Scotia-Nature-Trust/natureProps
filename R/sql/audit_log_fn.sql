CREATE OR REPLACE FUNCTION public.log_audit()
 RETURNS trigger
 LANGUAGE plpgsql
AS $function$
BEGIN
    IF (TG_OP = 'UPDATE') THEN
        INSERT INTO audit_log(table_name, operation, record_id, old_data, new_data, changed_by)
        VALUES (TG_TABLE_NAME, 'UPDATE', OLD.id::TEXT, row_to_json(OLD), row_to_json(NEW), current_setting('app.current_user', true));
        RETURN NEW;
    ELSIF (TG_OP = 'DELETE') THEN
        INSERT INTO audit_log(table_name, operation, record_id, old_data, changed_by)
        VALUES (TG_TABLE_NAME, 'DELETE', OLD.id::TEXT, row_to_json(OLD), current_setting('app.current_user', true));
        RETURN OLD;
    ELSIF (TG_OP = 'INSERT') THEN
        INSERT INTO audit_log(table_name, operation, record_id, new_data, changed_by)
        VALUES (TG_TABLE_NAME, 'INSERT', NEW.id::TEXT, row_to_json(NEW), current_setting('app.current_user', true));
        RETURN NEW;
    END IF;
END;
$function$
