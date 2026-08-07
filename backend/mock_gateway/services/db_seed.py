import json
import uuid
from datetime import date

from sqlalchemy.exc import IntegrityError

from models import AppUserProfile, DictionaryItem, Location, Person, RuntimeUserContext

_DICTIONARY_ENTRIES = {
    # Must match exactly the vocabulary _normalize_status_input()/_STATUS_TRANSITIONS
    # (api/gateway_validators.py, api/gateway_canonical_api.py) actually accept. This
    # domain used to list SUBMITTED/DONE/REJECTED too, implying more lifecycle states
    # than the app logic supports - reference data promising a status transition that
    # SetChecklistStatus would then reject with "Unsupported status".
    "STATUS": [
        ("DRAFT", "Черновик"),
        ("REGISTERED", "Зарегистрирован"),
        ("CLOSED", "Закрыт"),
    ],
    "LPC": [
        ("LPC-01", "ЛПК 01"),
        ("LPC-02", "ЛПК 02"),
        ("LPC-03", "ЛПК 03"),
        ("LPC-04", "ЛПК 04"),
        ("LPC-05", "ЛПК 05"),
    ],
    "PROFESSION": [
        ("OP", "Оператор"),
        ("SUP", "Супервайзер"),
        ("ENG", "Инженер"),
        ("TEC", "Техник"),
        ("MNT", "Механик"),
    ],
    "ATF_CAT": [
        ("GEN", "Прочее"),
        ("PHOTO", "Фото"),
        ("REPORT", "Отчет"),
        ("DOC", "Документ"),
    ],
    "TIME_ZONE": [
        ("UTC", "UTC"),
        ("Europe/Kaliningrad", "Europe/Kaliningrad"),
        ("Europe/Samara", "Europe/Samara"),
        ("Europe/Kirov", "Europe/Kirov"),
        ("Europe/Volgograd", "Europe/Volgograd"),
        ("Asia/Novosibirsk", "Asia/Novosibirsk"),
        ("Asia/Barnaul", "Asia/Barnaul"),
        ("Asia/Tomsk", "Asia/Tomsk"),
        ("Asia/Irkutsk", "Asia/Irkutsk"),
        ("Asia/Yakutsk", "Asia/Yakutsk"),
        ("Asia/Vladivostok", "Asia/Vladivostok"),
        ("Asia/Sakhalin", "Asia/Sakhalin"),
        ("Asia/Magadan", "Asia/Magadan"),
        ("Asia/Kamchatka", "Asia/Kamchatka"),
        ("Europe/Amsterdam", "Europe/Amsterdam"),
        ("Europe/Moscow", "Москва"),
        ("Europe/Saratov", "Саратов"),
        ("Asia/Yekaterinburg", "Екатеринбург"),
        ("Asia/Omsk", "Омск"),
        ("Asia/Krasnoyarsk", "Красноярск"),
    ],
}

_PERSONS = [
    ("100001", "Иван", "Иванов", "Сергеевич", "Оператор", "Цех добычи", "OBS_100001"),
    ("100002", "Петр", "Петров", "Алексеевич", "Супервайзер", "Цех добычи", "OBS_100002"),
    ("100003", "Сергей", "Сидоров", "Николаевич", "Инженер", "Служба ППР", "OBS_100003"),
    ("100004", "Анна", "Кузнецова", "Игоревна", "Техник", "Лаборатория", "OBS_100004"),
    ("100005", "Ольга", "Смирнова", "Павловна", "Оператор", "Компрессорная", "OBS_100005"),
    ("100006", "Мария", "Васильева", "Андреевна", "Инженер", "Энергослужба", "OBS_100006"),
    ("100007", "Дмитрий", "Попов", "Владимирович", "Механик", "Ремонтный участок", "OBS_100007"),
    ("100008", "Алексей", "Новиков", "Ильич", "Оператор", "Цех подготовки", "OBS_100008"),
    ("100009", "Елена", "Федорова", "Романовна", "Супервайзер", "Цех подготовки", "OBS_100009"),
    ("100010", "Наталья", "Морозова", "Олеговна", "Техник", "Склад", "OBS_100010"),
    ("100011", "Артем", "Волков", "Евгеньевич", "Инженер", "Служба надежности", "OBS_100011"),
    ("100012", "Юлия", "Соловьева", "Максимовна", "Оператор", "Цех добычи", "OBS_100012"),
    ("100013", "Михаил", "Лебедев", "Денисович", "Механик", "Ремонтный участок", "OBS_100013"),
    ("100014", "Татьяна", "Козлова", "Викторовна", "Супервайзер", "Лаборатория", "OBS_100014"),
    ("100015", "Андрей", "Орлов", "Петрович", "Инженер", "Энергослужба", "OBS_100015"),
    ("100016", "Виктор", "Павлов", "Олегович", "Оператор", "Компрессорная", "OBS_100016"),
    ("100017", "Екатерина", "Семенова", "Ильинична", "Техник", "Склад", "OBS_100017"),
    ("100018", "Роман", "Голубев", "Сергеевич", "Оператор", "Цех подготовки", "OBS_100018"),
    ("100019", "Дарья", "Виноградова", "Алексеевна", "Инженер", "Служба ППР", "OBS_100019"),
    ("100020", "Никита", "Богданов", "Степанович", "Механик", "Ремонтный участок", "OBS_100020"),
]

_LOCATIONS = [
    ("LOC-RU", None, "Россия"),
    ("LOC-PRD", "LOC-RU", "Производственный комплекс"),
    ("LOC-PRD-01", "LOC-PRD", "Установка подготовки газа"),
    ("LOC-PRD-02", "LOC-PRD", "Компрессорная станция"),
    ("LOC-PRD-03", "LOC-PRD", "Энергоблок"),
    ("LOC-PRD-01-A", "LOC-PRD-01", "Линия подготовки 1"),
    ("LOC-PRD-01-B", "LOC-PRD-01", "Линия подготовки 2"),
    ("LOC-PRD-02-A", "LOC-PRD-02", "Компрессорный зал"),
    ("LOC-PRD-02-B", "LOC-PRD-02", "Маслосистема"),
    ("LOC-PRD-03-A", "LOC-PRD-03", "Щитовая"),
    ("LOC-PRD-03-B", "LOC-PRD-03", "Дизель-генератор"),
]

_USER_PROFILES = [
    ("admin", "Администратор системы", ["view", "edit", "delete"]),
    ("viewer", "Наблюдатель производства", ["view"]),
    ("editor", "Инженер редактирования", ["view", "edit"]),
    ("deleter", "Контролер удаления", ["view", "delete"]),
    ("operator", "Оператор смены", ["view", "edit"]),
]


def seed_reference_data(db) -> None:
    seed_dictionary_items(db)
    seed_persons(db)
    seed_locations(db)
    seed_user_profiles(db)
    seed_runtime_user_context(db)
    db.commit()


def seed_dictionary_items(db) -> None:
    for domain, entries in _DICTIONARY_ENTRIES.items():
        for key, text in entries:
            exists = (
                db.query(DictionaryItem)
                .filter(
                    DictionaryItem.domain == domain,
                    DictionaryItem.key == key,
                )
                .first()
            )
            if exists:
                if exists.text != text:
                    exists.text = text
                continue
            db.add(
                DictionaryItem(
                    id=str(uuid.uuid4()),
                    domain=domain,
                    key=key,
                    text=text,
                    begda=date(2000, 1, 1),
                    endda=None,
                )
            )


def seed_persons(db) -> None:
    for perner, first_name, last_name, middle_name, position, org_unit, integration_name in _PERSONS:
        existing = db.get(Person, perner)
        if existing:
            existing.first_name = first_name
            existing.last_name = last_name
            existing.middle_name = middle_name
            existing.position = position
            existing.org_unit = org_unit
            existing.integration_name = integration_name
            continue
        db.add(
            Person(
                perner=perner,
                first_name=first_name,
                last_name=last_name,
                middle_name=middle_name,
                position=position,
                org_unit=org_unit,
                integration_name=integration_name,
                begda=date(2000, 1, 1),
                endda=None,
            )
        )


def seed_locations(db) -> None:
    for node_id, parent_id, location_name in _LOCATIONS:
        existing = db.get(Location, node_id)
        if existing:
            existing.parent_id = parent_id
            existing.location_name = location_name
            continue
        db.add(
            Location(
                node_id=node_id,
                parent_id=parent_id,
                location_name=location_name,
                begda=date(2000, 1, 1),
                endda=None,
            )
        )


def seed_user_profiles(db) -> None:
    for uname, full_name, permissions in _USER_PROFILES:
        if uname == "admin":
            permissions = [
                {"code": "01", "scope": "all"},
                {"code": "02", "scope": "all"},
                {"code": "03", "scope": "all"},
                {"code": "06", "scope": "all"},
            ]
        elif uname == "viewer":
            permissions = [
                {"code": "03", "scope": "bukrs", "scopeValue": "1000"},
            ]
        elif uname == "editor":
            permissions = [
                {"code": "02", "scope": "bukrs", "scopeValue": "1000"},
                {"code": "03", "scope": "bukrs", "scopeValue": "1000"},
            ]
        elif uname == "deleter":
            permissions = [
                {"code": "03", "scope": "all"},
                {"code": "06", "scope": "bukrs", "scopeValue": "2000"},
            ]
        elif uname == "operator":
            permissions = [
                {"code": "01", "scope": "all"},
                {"code": "02", "scope": "all"},
                {"code": "03", "scope": "all"},
                {"code": "06", "scope": "all"},
            ]
        existing = db.get(AppUserProfile, uname)
        serialized = json.dumps(list(permissions), ensure_ascii=False)
        if existing:
            existing.full_name = full_name
            existing.permissions_json = serialized
            continue
        db.add(AppUserProfile(uname=uname, full_name=full_name, permissions_json=serialized))


def seed_runtime_user_context(db) -> None:
    row = db.get(RuntimeUserContext, "CURRENT")
    if row:
        if not str(row.uname or "").strip():
            row.uname = "operator"
        return
    # SAVEPOINT (not a plain flush) so a concurrent insert race on this fixed "CURRENT"
    # primary key can be rolled back in isolation, without discarding the dictionary/
    # persons/locations/profiles inserts seed_reference_data() already queued earlier in
    # this same session ahead of its single closing db.commit().
    try:
        with db.begin_nested():
            db.add(RuntimeUserContext(key="CURRENT", uname="operator"))
    except IntegrityError:
        pass
