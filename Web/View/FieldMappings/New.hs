module Web.View.FieldMappings.New where

import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.Wai (Request)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data NewView = NewView

instance View NewView where
    html NewView =
        [hsx|
        {pageHeaderHtml (tr "New field mapping") mempty}
        <div class="card maxw-600"><div class="card-body">
        <form method="POST" action={CreateFieldMappingAction} data-testid="field-mapping-form">
            {fieldMappingFormFields "" 100 "field" "" True}
            <button type="submit" class="btn btn-brand" data-testid="field-mapping-submit">{tr "Create"}</button>
        </form>
        </div></div>|]

fieldMappingFormFields :: (CurrentUserRecord ~ User, ?request :: Request) => Text -> Int -> Text -> Text -> Bool -> Html
fieldMappingFormFields facet rank kind key enabled =
    [hsx|
    <div class="mb-3">
        <label class="form-label">{tr "Facet name"}</label>
        <input name="facet" type="text" class="form-control" value={facet} placeholder="env" data-testid="mapping-facet" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Rank (lower wins)"}</label>
        <input name="rank" type="number" class="form-control" value={rank} data-testid="mapping-rank"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Kind"}</label>
        <select name="kind" class="form-select" data-testid="mapping-kind">
            {forEach ["field", "label", "attr"] (kindOption kind)}
        </select>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Key (alert field / label name / Assets attribute name)"}</label>
        <input name="key" type="text" class="form-control" value={key} placeholder="Environments" data-testid="mapping-key" required="required"/>
    </div>
    <div class="mb-3 form-check">
        <input name="enabled" type="checkbox" class="form-check-input" checked={enabled} data-testid="mapping-enabled"/>
        <label class="form-check-label">{tr "Enabled"}</label>
    </div>
|]

kindOption :: Text -> Text -> Html
kindOption selected value =
    if value == selected
        then [hsx|<option value={value} selected="selected">{value}</option>|]
        else [hsx|<option value={value}>{value}</option>|]
