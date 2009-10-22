create table posts (
    id integer primary key,
    title text,
    timestamp datetime,
    body text
);

create table categories (
    id integer primary key,
    title text
);

create table category_map (
    category integer,
    post integer
);

create table media (
    id integer primary key,
    mime text,
    name text,
    path text
);

create table media_map (
    post_id integer,
    media_id integer
);

create table openid_sessions (
    id text unique,
    addr text,
    session text
);

create table sessions (
    id text unique,
    addr text,
    identity text
);
