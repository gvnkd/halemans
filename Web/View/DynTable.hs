module Web.View.DynTable (DynTable (..), dynTableHtml) where

import Application.Service.DynTable
import qualified Data.Text as Text
import Network.HTTP.Types.URI (renderQuery)
import Web.View.Prelude

-- Generic dynamic table widget (sortable columns, per-column filter row,
-- column picker, offset pager with page-size selector). All state lives in
-- the GET query string: sort/pager links are plain anchors rebuilt from the
-- full state, filter/col/pageSize inputs autosubmit the wrapping form (page
-- resets to 1 since the form carries no page input). The per-page SQL
-- applies the same state server-side; see Application.Service.DynTable.
-- Embedded tables (dashboard cards, group members) disable picker/pager via
-- TableConfig and render no wrapping form at all.
--
-- RecordWildCards pattern-match: the dtRowHtml function field breaks
-- HasField selector magic on the record itself.
data DynTable row = DynTable
    { dtTestId :: Maybe Text
    , dtTbodyId :: Text
    , dtLiveScope :: Maybe Text
    , dtLiveFilters :: Maybe Text
    , dtTableClass :: Text
    , dtConfig :: TableConfig
    , dtState :: TableState
    , dtBasePath :: Text
    , dtResetUrl :: Maybe Text
    , dtExtraItems :: [(ByteString, Maybe ByteString)]
    -- ^ Extra query items every widget link/form keeps (e.g. the env
    -- page's view mode or a dashboard card's pinned value).
    , dtTotal :: Int64
    , dtRows :: [row]
    , dtRowHtml :: [TableColumn] -> row -> Html
    -- ^ Renders one <tr> for the given visible columns.
    , dtEmptyText :: Text
    -- ^ Designed empty state (one muted line) rendered as a full-width tbody
    -- row when there are no rows; CSS hides it as soon as live updates add
    -- real rows (tbody:has in static/app.css).
    }

dynTableHtml :: DynTable row -> Html
dynTableHtml DynTable{..} =
    if needsForm
        then
            [hsx|
            <form method="GET" action={dtBasePath} data-testid={testid "filters"}>
                {formHiddens}
                {body}
            </form>
        |]
        else body
  where
    cfg = dtConfig
    state = dtState
    name = cfg.cfgName
    testid suffix = name <> "-" <> suffix
    sortValue = state.tsSort
    dirValue = state.tsDir
    visible = visibleColumns cfg state
    hiddenFilters = hiddenFilterColumns cfg state
    needsForm = cfg.cfgColumnPicker || any (isJust . colFilter) cfg.cfgColumns || cfg.cfgPager

    body =
        [hsx|
        {toolbar}
        {tableHtml}
        {footer}
    |]

    formHiddens =
        [hsx|
        <input type="hidden" name="sort" value={sortValue}/>
        <input type="hidden" name="dir" value={dirValue}/>
        {forEach dtExtraItems extraHidden}
    |]
    extraHidden (key, Just value) = [hsx|<input type="hidden" name={keyText} value={valueText}/>|]
      where
        keyText = cs key :: Text
        valueText = cs value :: Text
    extraHidden (_, Nothing) = mempty

    stateUrl :: TableState -> Text
    stateUrl st = dtBasePath <> cs (renderQuery True (dtExtraItems ++ tableStateQueryItems cfg st))

    headerCell col
        | col.colSortable =
            [hsx|
                <th><a href={sortUrl col} class="text-decoration-none" data-testid={"sort-" <> col.colKey}>{tr col.colLabel}{sortIndicator col}</a></th>
            |]
        | otherwise = [hsx|<th>{tr col.colLabel}</th>|]
    sortUrl col = stateUrl state{tsSort = col.colKey, tsDir = nextSortDirFor cfg state col.colKey, tsPage = 1}
    sortIndicator col =
        if state.tsSort == col.colKey
            then [hsx|<span class="sort-indicator">{arrow}</span>|]
            else mempty
      where
        arrow :: Text
        arrow = if state.tsDir == "asc" then " ▲" else " ▼"

    filterRow =
        if any (isJust . colFilter) visible
            then [hsx|<tr class="dyn-filter-row">{forEach visible filterCell}</tr>|]
            else mempty
    filterCell col = case col.colFilter of
        Nothing -> [hsx|<th></th>|]
        Just cf -> [hsx|<th class="dyn-filter-cell">{filterInput col cf}</th>|]
    filterInput col cf = case cf.cfKind of
        FilterText -> textFilter cf
        FilterMulti -> multiFilter col cf

    textFilter cf =
        [hsx|
            <input name={cf.cfParam} class="form-control form-control-sm" placeholder={tr cf.cfPlaceholder} value={currentText cf} list={listId cf} autocomplete="off" data-autosubmit="" data-testid={"filter-" <> cf.cfParam}/>
            {suggestionList cf}
        |]
    listId cf = "filter-suggestions-" <> cf.cfParam
    currentText cf = fromMaybe "" (listToMaybe (filterValues state cf.cfParam))
    suggestionList cf =
        if null cf.cfOptions
            then mempty
            else [hsx|<datalist id={listId cf}>{forEach cf.cfOptions suggestionOption}</datalist>|]
    suggestionOption suggestion = [hsx|<option value={suggestion}></option>|]

    multiFilter col cf =
        [hsx|
        <div class="dropdown" data-testid={"filter-" <> cf.cfParam} data-filter-dropdown="true">
            <button class="btn btn-sm btn-ghost dropdown-toggle" type="button" data-bs-toggle="dropdown" data-bs-auto-close="outside">{buttonLabel}</button>
            <div class="dropdown-menu p-2">
                {forEach cf.cfOptions optionItem}
            </div>
        </div>
    |]
      where
        selected = filterValues state cf.cfParam
        buttonLabel :: Text
        buttonLabel = trp "{label}: {selected}" [("label", Text.toLower (tr col.colLabel)), ("selected", selectedText)]
        selectedText :: Text
        selectedText = if null selected then tr "any" else tshow (length selected)
        optionItem value =
            [hsx|
                <div class="form-check">
                    <input class="form-check-input" type="checkbox" name={cf.cfParam} value={value} id={optionId value} checked={value `elem` selected}/>
                    <label class="form-check-label" for={optionId value}>{value}</label>
                </div>
            |]
        optionId value = cf.cfParam <> "-" <> value

    toolbar =
        if showToolbar
            then
                [hsx|
                <div class="d-flex flex-wrap gap-2 mb-2 align-items-center">
                    {columnPicker}
                    {forEach hiddenFilters hiddenFilterInput}
                    {resetLink}
                </div>
            |]
            else mempty
    showToolbar = cfg.cfgColumnPicker || not (null hiddenFilters) || isJust dtResetUrl

    columnPicker =
        if cfg.cfgColumnPicker
            then
                [hsx|
                <div class="dropdown" data-testid={testid "cols"} data-filter-dropdown="true">
                    <button class="btn btn-sm btn-ghost dropdown-toggle" type="button" data-bs-toggle="dropdown" data-bs-auto-close="outside">{pickerLabel}</button>
                    <div class="dropdown-menu p-2">
                        {forEach cfg.cfgColumns columnOption}
                    </div>
                </div>
            |]
            else mempty
      where
        pickerLabel :: Text
        pickerLabel = trp "Columns: {visible}/{total}" [("visible", tshow (length visible)), ("total", tshow (length cfg.cfgColumns))]
        columnOption col =
            [hsx|
                <div class="form-check">
                    <input class="form-check-input" type="checkbox" name="cols" value={col.colKey} id={colId col} checked={col.colKey `elem` state.tsVisible}/>
                    <label class="form-check-label" for={colId col}>{tr col.colLabel}</label>
                </div>
            |]
        colId col = name <> "-cols-" <> col.colKey

    -- Hidden-but-filterable columns keep their inputs here so a hidden
    -- column's filter stays reachable (e.g. the alerts group filter).
    hiddenFilterInput col = case col.colFilter of
        Nothing -> mempty
        Just cf -> case cf.cfKind of
            FilterMulti -> multiFilter col cf
            FilterText ->
                [hsx|
                <div class="d-flex align-items-center gap-1" data-testid={testid ("filter-" <> cf.cfParam)}>
                    <span class="text-muted small">{tr col.colLabel}</span>
                    {textFilter cf}
                </div>
            |]

    resetLink = case dtResetUrl of
        Just url -> [hsx|<a href={url} class="btn btn-sm btn-ghost" data-testid={testid "filters-reset"}>{tr "Reset"}</a>|]
        Nothing -> mempty

    emptyRow =
        if null dtRows
            then
                [hsx|
                <tr class="dyn-empty-row" data-testid={testid "empty"}>
                    <td colspan={colspan}>{dtEmptyText}</td>
                </tr>
                |]
            else mempty
    colspan = tshow (max 1 (length visible)) :: Text

    tableHtml =
        [hsx|
        <table class={dtTableClass} data-testid={dtTestId} data-live-scope={dtLiveScope} data-live-filters={dtLiveFilters}>
            <thead>
                <tr>
                    {forEach visible headerCell}
                </tr>
                {filterRow}
            </thead>
            <tbody id={dtTbodyId}>
                {forEach dtRows (dtRowHtml visible)}
                {emptyRow}
            </tbody>
        </table>
    |]

    footer =
        if cfg.cfgPager
            then
                [hsx|
                <div class="d-flex flex-wrap justify-content-between align-items-center mt-2 gap-2">
                    {rangeInfo}
                    {pagerNav}
                    {pageSizeSelect}
                </div>
            |]
            else mempty

    firstRow =
        if dtTotal == 0
            then 0
            else (state.tsPage - 1) * state.tsPageSize + 1
    lastRow = firstRow + length dtRows - 1
    rangeInfo =
        [hsx|
            <span class="text-muted" data-testid={testid "range"}>{rangeText}</span>
        |]
    rangeText :: Text
    rangeText = trp "{first}–{last} of {total}" [("first", tshow firstRow), ("last", tshow lastRow), ("total", tshow dtTotal)]

    totalPages = pageCountFor dtTotal state.tsPageSize
    pagerNav =
        if totalPages <= 1
            then mempty
            else
                [hsx|
                <nav data-testid={testid "pages"}>
                    <ul class="pagination pagination-sm mb-0">
                        {prevItem}
                        {forEach (pageWindow state.tsPage totalPages) pageItem}
                        {nextItem}
                    </ul>
                </nav>
            |]
    pageUrl p = stateUrl state{tsPage = p}
    pageItem Nothing = [hsx|<li class="page-item disabled"><span class="page-link">…</span></li>|]
    pageItem (Just p) =
        [hsx|
            <li class={pageClass p}><a class="page-link" href={pageUrl p} data-testid={pageTestId p}>{p}</a></li>
        |]
    pageTestId p = testid ("page-" <> tshow p)
    pageClass :: Int -> Text
    pageClass p = "page-item" <> if p == state.tsPage then " active" else ""
    prevItem
        | state.tsPage <= 1 = [hsx|<li class="page-item disabled"><span class="page-link">‹</span></li>|]
        | otherwise =
            [hsx|
                <li class="page-item"><a class="page-link" href={pageUrl (state.tsPage - 1)} data-testid={testid "page-prev"}>‹</a></li>
            |]
    nextItem
        | state.tsPage >= totalPages = [hsx|<li class="page-item disabled"><span class="page-link">›</span></li>|]
        | otherwise =
            [hsx|
                <li class="page-item"><a class="page-link" href={pageUrl (state.tsPage + 1)} data-testid={testid "page-next"}>›</a></li>
            |]

    pageSizeSelect =
        [hsx|
        <select name="pageSize" class="form-select form-select-sm w-auto" data-autosubmit="" data-testid={testid "page-size"}>
            {forEach cfg.cfgPageSizes sizeOption}
        </select>
    |]
      where
        sizeOption n = [hsx|<option value={sizeValue n} selected={n == state.tsPageSize}>{sizeLabel n}</option>|]
        sizeValue n = tshow n :: Text
        sizeLabel n = trp "{n} / page" [("n", tshow n)]
