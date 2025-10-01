CREATE TABLE IF NOT EXISTS users (
    id serial primary key,
    username text not null unique,
    password text not null,
    updated_at timestamp not null default now(),
    created_at timestamp not null default now(),
    last_login_at timestamp null
);

CREATE TABLE IF NOT EXISTS directories (
    id serial primary key,
    directory text not null unique
);

CREATE TABLE IF NOT EXISTS directory_users (
    id serial primary key,
    directory_id integer references directories on delete cascade,
    user_id integer references users on delete cascade
);

CREATE TABLE IF NOT EXISTS events (
    id serial primary key,
    event_type text not null,
    event_details text null,
    created_at timestamp not null
);
