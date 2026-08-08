@AbapCatalog.sqlViewName: 'ZICHKROOTSQL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Check: Root (composite push-down — KPI/criticality/hidden)'
@Search.searchable: true

// Аналог compute_check_root_view() в serve.py — весь push-down (KPI-счётчики,
// criticality, hidden-флаги, Basic-proxy поля) считается здесь, на чтении,
// а не хранится денормализованно на ZCHK_ROOT. Соответствует принципу
// Code-to-Data: вычисление едет к данным (HANA), а не данные — к вычислению
// (ABAP-цикл по строкам).
//
// [EMPIRICAL FIX] Basic-поля (Date/Time/.../ProfText) спроецированы СЮДА,
// на CheckRoot, а не читаются клиентом через to_Basic-навигацию — SAPUI5
// 1.71 + OData v2 не отдавал BindingContext для to_Basic-facet у transient
// (ещё не сохранённого) CheckRoot (см. ext/util/ObjectPageExtension и
// annotations.xml, комментарий "EMPIRICAL FIX" на FieldGroup#General).
// SSOT по значению — всё равно ZCHK_BASIC (см. ZI_CheckBasic), запись через
// эти поля маршрутизирует ZCL_CHECK_DPC_EXT в CheckBasics, не в CheckRoots.
//
// ЧЕГО ЗДЕСЬ НЕТ (сознательно, см. abap/README.md "Что не может CDS"):
// HeaderKpiTitle/HeaderKpiSubtitle — не форматируются в CDS. Конкатенация
// "КПР: %s · Профессия: %s" технически возможна через вложенные concat(),
// но результат нечитаем и хрупок к правкам формата; тот же формат уже
// параллельно живёт в ext/util/Constants.js/i18n.properties для
// transient-объекта — как СТРОКОВОЕ ФОРМАТИРОВАНИЕ, а не PUSH-DOWN
// ВЫЧИСЛЕНИЕ, это осознанно отдано ZCL_CHECK_DPC_EXT=>GET_ENTITY (после
// SELECT из этого view). Здесь для них — только placeholder-'' (перезаписывается
// DPC_EXT перед отправкой клиенту).
//
// [Фаза 5, NW 7.50/pre-S4 1610] Целевая система не поддерживает
// IsActiveEntity как часть OData-ключа "из коробки" (это более поздняя
// стандартизация BOPF/S4) — draft собирается вручную через UNION ALL
// активной (zchk_root) и черновиковой (zchk_root_d) таблиц, классический
// приём тех лет (пример в системе — SEPMRA_PD_D). ActiveUUID/DraftUUID —
// технические GUID-ключи адресации экземпляра; RootId остаётся стабильным
// бизнес-ключом (FK у CheckBasic/CheckItem/Barrier не меняется).
define view ZI_CheckRoot
  as select from zchk_root as Root

    left outer join ZI_CheckBasic as Basic
      on Root.root_id = Basic.RootId

    left outer join ZI_CheckItemAgg as Checks
      on Root.root_id = Checks.RootId

    left outer join ZI_BarrierAgg as Barriers
      on Root.root_id = Barriers.RootId

    // Активной строке нужно знать, есть ли у неё незавершённый edit-черновик
    // (HasDraftEntity) — LEFT OUTER JOIN, а не EXISTS-подзапрос, чтобы не
    // плодить коррелированный подзапрос на каждую строку (Code-to-Data).
    left outer join zchk_root_d as OwnDraft
      on Root.root_id = OwnDraft.active_uuid
{
  key Root.root_id                                                as RootId,
      Root.root_id                                                as ActiveUUID,
      cast( '00000000000000000000000000000000' as abap.char( 32 ) )
                                                                    as DraftUUID,
      cast( abap_true as abap.char( 1 ) )                         as IsActiveEntity,
      cast( abap_true as abap.char( 1 ) )                         as HasActiveEntity,
      case when OwnDraft.draft_uuid is not null then abap_true else abap_false end
                                                                    as HasDraftEntity,
      Root.doc_id                                                 as DocId,

      // --- Basic-proxy (см. комментарий выше и ZI_CheckBasic) ---
      Basic.Date                                                  as Date,
      Basic.Time                                                  as Time,
      Basic.Timezone                                              as Timezone,
      Basic.TimezoneText                                          as TimezoneText,
      Basic.LocationKey                                           as LocationKey,
      Basic.LocationName                                          as LocationName,
      Basic.Equipment                                             as Equipment,
      Basic.ObserverFullname                                      as ObserverFullname,
      Basic.ObserverPerner                                        as ObserverPerner,
      Basic.ObservedFullname                                      as ObservedFullname,
      Basic.ObservedPerner                                        as ObservedPerner,
      Basic.LpcKey                                                as LpcKey,
      Basic.LpcText                                                as LpcText,
      Basic.ProfKey                                                as ProfKey,
      Basic.ProfText                                              as ProfText,

      Root.status                                                 as Status,
      case Root.status
        when 'CRITICAL' then 1
        when 'WARNING'  then 2
        else 3
      end                                                          as StatusCriticality,

      @Semantics.booleanFlag: true
      Root.this_is_integration_data                               as ThisIsIntegrationData,

      // --- KPI-агрегаты (push-down из ZI_CheckItemAgg/ZI_BarrierAgg) ---
      coalesce( Checks.ChecksAmount, 0 )                          as ChecksAmount,
      coalesce( Checks.ChecksSuccessCount, 0 )                    as ChecksSuccess,
      coalesce( Barriers.BarriersAmount, 0 )                      as BarriersAmount,
      coalesce( Barriers.BarriersSuccessCount, 0 )                as BarriersSuccess,

      // Percentage computed once in ZI_CheckItemAgg/ZI_BarrierAgg
      // (ChecksSuccessRate/BarriersSuccessRate) — reused here for both
      // SuccessRate* and *Criticality, no duplicated formula.
      coalesce( Checks.ChecksSuccessRate, cast( null as abap.dec( 5, 2 ) ) )     as SuccessRateChecks,
      coalesce( Barriers.BarriersSuccessRate, cast( null as abap.dec( 5, 2 ) ) ) as SuccessRateBarriers,

      // Пороги 50%/80% — BUSINESS_RULES.CRITICALITY_NEGATIVE_MAX/
      // CRITICALITY_CRITICAL_MAX в serve_config.py; 0 при пустой коллекции
      // (ChecksSuccessRate = null, см. ZI_CheckItemAgg).
      case when Checks.ChecksSuccessRate is null then 0
           when Checks.ChecksSuccessRate < 50 then 1
           when Checks.ChecksSuccessRate < 80 then 2
           else 3
      end                                                          as ChecksCriticality,

      case when Barriers.BarriersSuccessRate is null then 0
           when Barriers.BarriersSuccessRate < 50 then 1
           when Barriers.BarriersSuccessRate < 80 then 2
           else 3
      end                                                          as BarriersCriticality,

      @Semantics.booleanFlag: true
      case when coalesce( Checks.ChecksErrorCount, 0 ) > 0 then abap_true else abap_false end
                                                                    as HasErrorChecks,

      @Semantics.booleanFlag: true
      case when coalesce( Barriers.BarriersErrorCount, 0 ) > 0 then abap_true else abap_false end
                                                                    as HasErrorBarriers,

      // --- Placeholder — форматируется в ZCL_CHECK_DPC_EXT, см. комментарий сверху ---
      cast( '' as abap.char( 200 ) )                              as HeaderKpiTitle,
      cast( '' as abap.char( 200 ) )                              as HeaderKpiSubtitle,

      // --- Hidden-флаги секций (правило по LpcKey, push-down; для
      // transient-объекта ДО первого save это правило дублируется на
      // клиенте — см. ext/util/Constants.js TRANSIENT_UX_RULES) ---
      @Semantics.booleanFlag: true
      case when coalesce( Basic.LpcKey, '' ) = ''
        then abap_true else abap_false
      end                                                          as ChecksHidden,

      @Semantics.booleanFlag: true
      case when coalesce( Basic.LpcKey, '' ) in ( '', '0', '1' )
        then abap_true else abap_false
      end                                                          as BarriersHidden,

      @Semantics.booleanFlag: true
      case when Root.this_is_integration_data = abap_true then abap_false else abap_true end
                                                                    as IntegrationBadgeHidden,

      @Semantics.booleanFlag: true
      case when coalesce( Checks.ChecksErrorCount, 0 ) > 0 then abap_false else abap_true end
                                                                    as ChecksErrorBadgeHidden,

      @Semantics.booleanFlag: true
      case when coalesce( Barriers.BarriersErrorCount, 0 ) > 0 then abap_false else abap_true end
                                                                    as BarriersErrorBadgeHidden,

      // ETag выставляется ZCL_CHECK_DPC_EXT через ZCL_CHECK_ROOT_ETAG_AMDP
      // (max(LastChangedAt) по Root+Basic+все Items+все Barriers) — здесь
      // Root.last_changed_at ТОЛЬКО как fallback-значение поля, а не как
      // реальный ETag composite-представления. См. abap/README.md.
      Root.last_changed_at                                        as LastChangedAt,
      Root.created_by                                             as CreatedBy,
      Root.created_at                                             as CreatedAt
}

// [Фаза 5] Черновиковая половина UNION ALL. RootId для ещё-не-активированного
// create-драфта (active_uuid is null) — собственный DraftUUID черновика: это
// и есть значение, которое FK-полем "RootId" видят CheckBasic/CheckItem/
// Barrier ДО активации (после — Root.root_id перезаписывает его на активный).
// Колонки строго в том же порядке/типах, что в активной половине выше —
// иначе `union all` не скомпилируется.
union all
  select from zchk_root_d as Draft

    left outer join ZI_CheckBasic as Basic
      on coalesce( Draft.active_uuid, Draft.draft_uuid ) = Basic.RootId

    left outer join ZI_CheckItemAgg as Checks
      on coalesce( Draft.active_uuid, Draft.draft_uuid ) = Checks.RootId

    left outer join ZI_BarrierAgg as Barriers
      on coalesce( Draft.active_uuid, Draft.draft_uuid ) = Barriers.RootId
{
  key coalesce( Draft.active_uuid, Draft.draft_uuid )             as RootId,
      coalesce( Draft.active_uuid,
        cast( '00000000000000000000000000000000' as abap.char( 32 ) ) )
                                                                    as ActiveUUID,
      Draft.draft_uuid                                            as DraftUUID,
      cast( abap_false as abap.char( 1 ) )                        as IsActiveEntity,
      case when Draft.active_uuid is not null then abap_true else abap_false end
                                                                    as HasActiveEntity,
      cast( abap_false as abap.char( 1 ) )                        as HasDraftEntity,
      Draft.doc_id                                                as DocId,

      Basic.Date                                                  as Date,
      Basic.Time                                                  as Time,
      Basic.Timezone                                              as Timezone,
      Basic.TimezoneText                                          as TimezoneText,
      Basic.LocationKey                                           as LocationKey,
      Basic.LocationName                                          as LocationName,
      Basic.Equipment                                             as Equipment,
      Basic.ObserverFullname                                      as ObserverFullname,
      Basic.ObserverPerner                                        as ObserverPerner,
      Basic.ObservedFullname                                      as ObservedFullname,
      Basic.ObservedPerner                                        as ObservedPerner,
      Basic.LpcKey                                                as LpcKey,
      Basic.LpcText                                                as LpcText,
      Basic.ProfKey                                                as ProfKey,
      Basic.ProfText                                              as ProfText,

      Draft.status                                                as Status,
      case Draft.status
        when 'CRITICAL' then 1
        when 'WARNING'  then 2
        else 3
      end                                                          as StatusCriticality,

      @Semantics.booleanFlag: true
      Draft.this_is_integration_data                              as ThisIsIntegrationData,

      coalesce( Checks.ChecksAmount, 0 )                          as ChecksAmount,
      coalesce( Checks.ChecksSuccessCount, 0 )                    as ChecksSuccess,
      coalesce( Barriers.BarriersAmount, 0 )                      as BarriersAmount,
      coalesce( Barriers.BarriersSuccessCount, 0 )                as BarriersSuccess,

      coalesce( Checks.ChecksSuccessRate, cast( null as abap.dec( 5, 2 ) ) )     as SuccessRateChecks,
      coalesce( Barriers.BarriersSuccessRate, cast( null as abap.dec( 5, 2 ) ) ) as SuccessRateBarriers,

      case when Checks.ChecksSuccessRate is null then 0
           when Checks.ChecksSuccessRate < 50 then 1
           when Checks.ChecksSuccessRate < 80 then 2
           else 3
      end                                                          as ChecksCriticality,

      case when Barriers.BarriersSuccessRate is null then 0
           when Barriers.BarriersSuccessRate < 50 then 1
           when Barriers.BarriersSuccessRate < 80 then 2
           else 3
      end                                                          as BarriersCriticality,

      @Semantics.booleanFlag: true
      case when coalesce( Checks.ChecksErrorCount, 0 ) > 0 then abap_true else abap_false end
                                                                    as HasErrorChecks,

      @Semantics.booleanFlag: true
      case when coalesce( Barriers.BarriersErrorCount, 0 ) > 0 then abap_true else abap_false end
                                                                    as HasErrorBarriers,

      cast( '' as abap.char( 200 ) )                              as HeaderKpiTitle,
      cast( '' as abap.char( 200 ) )                              as HeaderKpiSubtitle,

      @Semantics.booleanFlag: true
      case when coalesce( Basic.LpcKey, '' ) = ''
        then abap_true else abap_false
      end                                                          as ChecksHidden,

      @Semantics.booleanFlag: true
      case when coalesce( Basic.LpcKey, '' ) in ( '', '0', '1' )
        then abap_true else abap_false
      end                                                          as BarriersHidden,

      @Semantics.booleanFlag: true
      case when Draft.this_is_integration_data = abap_true then abap_false else abap_true end
                                                                    as IntegrationBadgeHidden,

      @Semantics.booleanFlag: true
      case when coalesce( Checks.ChecksErrorCount, 0 ) > 0 then abap_false else abap_true end
                                                                    as ChecksErrorBadgeHidden,

      @Semantics.booleanFlag: true
      case when coalesce( Barriers.BarriersErrorCount, 0 ) > 0 then abap_false else abap_true end
                                                                    as BarriersErrorBadgeHidden,

      Draft.last_changed_at                                       as LastChangedAt,
      Draft.created_by                                            as CreatedBy,
      Draft.created_at                                            as CreatedAt
}
