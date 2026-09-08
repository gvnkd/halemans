module Web.View.FieldMappings.Edit where
import Web.View.Prelude
import Web.View.FieldMappings.New (fieldMappingFormFields)

data EditView = EditView { mapping :: FieldMapping }

instance View EditView where
    html EditView { .. } = [hsx|
        <h1>Edit field mapping</h1>
        <form method="POST" action={UpdateFieldMappingAction mapping.id} data-testid="field-mapping-edit-form" style="max-width: 600px">
            {fieldMappingFormFields mapping.facet mapping.rank mapping.kind mapping.key mapping.enabled}
            <button type="submit" class="btn btn-primary" data-testid="field-mapping-submit">Save</button>
        </form>
    |]
