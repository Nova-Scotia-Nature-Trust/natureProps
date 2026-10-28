# Apply triggers for log_audit SQL function
DBI::dbExecute(
  db_con,
  statement = "CREATE TRIGGER properties_audit
    AFTER INSERT OR UPDATE OR DELETE ON properties
    FOR EACH ROW EXECUTE FUNCTION log_audit();
  "
)

DBI::dbExecute(
  db_con,
  statement = "CREATE TRIGGER parcels_audit
    AFTER INSERT OR UPDATE OR DELETE ON parcels
    FOR EACH ROW EXECUTE FUNCTION log_audit();
  "
)

DBI::dbExecute(
  db_con,
  statement = "CREATE TRIGGER outreach_audit
    AFTER INSERT OR UPDATE OR DELETE ON outreach
    FOR EACH ROW EXECUTE FUNCTION log_audit();
  "
)

DBI::dbExecute(
  db_con,
  statement = "CREATE TRIGGER property_contact_audit
    AFTER INSERT OR UPDATE OR DELETE ON property_contact_details
    FOR EACH ROW EXECUTE FUNCTION log_audit();
  "
)

DBI::dbExecute(
  db_con,
  statement = "CREATE TRIGGER contact_communication_audit
    AFTER INSERT OR UPDATE OR DELETE ON property_contact_communication
    FOR EACH ROW EXECUTE FUNCTION log_audit();
  "
)

DBI::dbExecute(
  db_con,
  statement = "CREATE TRIGGER securement_action_items_audit
    AFTER INSERT OR UPDATE OR DELETE ON securement_action_items
    FOR EACH ROW EXECUTE FUNCTION log_audit();
  "
)

# Check
DBI::dbGetQuery(db_con, "SELECT * FROM information_schema.triggers;")
