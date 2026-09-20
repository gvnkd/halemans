module Web.View.FieldMappings.Edit where

import Web.View.FieldMappings.New (fieldMappingFormFields)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data EditView = EditView {mapping :: FieldMapping}

instance View EditView where
    html EditView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit field mapping") mempty}
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={UpdateFieldMappingAction mapping.id} data-testid="field-mapping-edit-form">
            {fieldMappingFormFields mapping.facet mapping.rank mapping.kind mapping.key mapping.enabled}
            <button type="submit" class="btn btn-brand" data-testid="field-mapping-submit">{tr "Save"}</button>
        </form>
        </div></div>|]
