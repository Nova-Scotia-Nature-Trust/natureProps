CREATE OR REPLACE FUNCTION public.log_audit()
 RETURNS trigger
 LANGUAGE plpgsql
AS $function$
BEGIN
    IF (TG_OP = 'UPDATE') THEN
        INSERT INTO audit_log(table_name, operation, record_id, old_data, new_data)
        VALUES (TG_TABLE_NAME, 'UPDATE', OLD.id::TEXT, row_to_json(OLD), row_to_json(NEW));
        RETURN NEW;
    ELSIF (TG_OP = 'DELETE') THEN
        INSERT INTO audit_log(table_name, operation, record_id, old_data)
        VALUES (TG_TABLE_NAME, 'DELETE', OLD.id::TEXT, row_to_json(OLD));
        RETURN OLD;
    ELSIF (TG_OP = 'INSERT') THEN
        INSERT INTO audit_log(table_name, operation, record_id, new_data)
        VALUES (TG_TABLE_NAME, 'INSERT', NEW.id::TEXT, row_to_json(NEW));
        RETURN NEW;
    END IF;
END;
$function$
