module Web.View.FieldMappings.Edit where

import Web.View.FieldMappings.New (fieldMappingFormFields)
import Web.View.Prelude

data EditView = EditView {mapping :: FieldMapping}

instance View EditView where
    html EditView{..} =
        [hsx|
        <h1>{tr "Edit field mapping"}</h1>
        <form method="POST" action={UpdateFieldMappingAction mapping.id} data-testid="field-mapping-edit-form" class="maxw-600">
            {fieldMappingFormFields mapping.facet mapping.rank mapping.kind mapping.key mapping.enabled}
            <button type="submit" class="btn btn-primary" data-testid="field-mapping-submit">{tr "Save"}</button>
        </form>
    |]
