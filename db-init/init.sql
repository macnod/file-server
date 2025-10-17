--
-- schema
--

create extension if not exists "uuid-ossp";

\c fileserver

--
-- role-based access control (rbac) tables
-- 

-- users
create table users (
    id uuid primary key default uuid_generate_v4(),
    created_at timestamp not null default now(),
    updated_at timestamp not null default now(),
    deleted_at timestamp default null,
    updated_by uuid not null references users(id),
    last_login timestamp,
    username text not null unique,
    password_hash text not null,
    email text not null
);

-- permissions
create table permissions (
    id uuid primary key default uuid_generate_v4(),
    created_at timestamp not null default now(),
    updated_at timestamp not null default now(),
    deleted_at timestamp default null,
    updated_by uuid not null references users(id) on delete cascade,
    permission_name text not null unique,
    permission_description text
);

-- Roles. Permissions are associated with roles only, never directly with a
-- user. Each user has a role that is associated with that user only, that has a
-- role_name in the format "{username}:exclusive", that has a role_description
-- that reads "Exclusive role for user {username).", and that has the
-- exclusive field set to true.
create table roles (
    id uuid primary key default uuid_generate_v4(),
    created_at timestamp not null default now(),
    updated_at timestamp not null default now(),
    deleted_at timestamp default null,
    updated_by uuid not null references users(id) on delete cascade,
    role_name text not null unique,
    role_description text,
    exclusive boolean not null default false
);

-- Resources. Things that require permissions for a user to be able to access
-- them.
create table resources (
    id uuid primary key default uuid_generate_v4(),
    created_at timestamp not null default now(),
    updated_at timestamp not null default now(),
    deleted_at timestamp default null,
    updated_by uuid not null references users(id) on delete cascade,
    resource_name text not null unique,
    resource_description text
);

-- Permissions associated with each role
create table role_permissions(
    id uuid primary key default uuid_generate_v4(),
    created_at timestamp not null default now(),
    updated_at timestamp not null default now(),
    deleted_at timestamp default null,
    updated_by uuid not null references users(id) on delete cascade,
    role_id uuid not null references roles(id) on delete cascade,
    permission_id uuid not null references permissions(id) on delete cascade,
    unique(role_id, permission_id)
);

-- Users associated with each role. A role with exclusive = true can have only
-- one user. Other roles can have unlimited users.
create table role_users (
    id uuid primary key default uuid_generate_v4(),
    created_at timestamp not null default now(),
    updated_at timestamp not null default now(),
    deleted_at timestamp default null,
    updated_by uuid not null references users(id) on delete cascade,
    role_id uuid not null references roles(id) on delete cascade,
    user_id uuid not null references users(id) on delete cascade,
    unique(user_id, role_id)
);

-- Roles and permissions associated with a resource.
create table resource_roles (
    id uuid primary key default uuid_generate_v4(),
    created_at timestamp not null default now(),
    updated_at timestamp not null default now(),
    deleted_at timestamp default null,
    updated_by uuid not null references users(id) on delete cascade,
    resource_id uuid not null references resources(id) on delete cascade,
    role_id uuid not null references roles(id) on delete cascade,
    unique(resource_id, role_id)
);

--
-- Indexes for soft deletes
--

-- users table
create index idx_users_deleted_at on users(deleted_at);

-- permissions table
create index idx_permissions_deleted_at on permissions(deleted_at);

-- roles table
create index idx_roles_deleted_at on roles(deleted_at);

-- resources table
create index idx_resources_deleted_at on resources(deleted_at);

-- role_permissions table
create index idx_role_permissions_deleted_at on role_permissions(deleted_at);

-- role_users table
create index idx_role_users_deleted_at on role_users(deleted_at);

-- resource_roles table
create index idx_resource_roles_deleted_at on resource_roles(deleted_at);

--
-- Partial indexes for soft deletes
--

create index idx_users_active on users(username) where deleted_at is null;
create index idx_roles_active on roles(role_name) where deleted_at is null;
create index idx_permissions_active on permissions(permission_name) 
    where deleted_at is null;
create index idx_resources_active on resources(resource_name) 
    where deleted_at is null;


--
-- Triggers for updated_at column
--

-- Function for trigger that will set the updated_at column to the current time
-- whenever a row is updated in the table.
create or replace function set_updated_at_column()
returns trigger as $$
begin
    new.updated_at = now();
    return new;
end;
$$ language 'plpgsql';

-- users trigger
create trigger set_users_updated_at
before update on users
for each row
execute function set_updated_at_column();

-- permissions trigger
create trigger set_permissions_updated_at
before update on permissions
for each row
execute function set_updated_at_column();

-- roles trigger
create trigger set_roles_updated_at
before update on roles
for each row
execute function set_updated_at_column();

-- resources trigger
create trigger set_resources_updated_at
before update on resources
for each row
execute function set_updated_at_column();

-- role_permissions trigger
create trigger set_role_permissions_updated_at
before update on role_permissions
for each row
execute function set_updated_at_column();

-- role_users trigger
create trigger set_role_users_updated_at
before update on role_users
for each row
execute function set_updated_at_column();

-- resource_roles trigger
create trigger set_resource_roles_updated_at
before update on resource_roles
for each row
execute function set_updated_at_column();

-- Function to enforce exclusive role constraints:
-- 1. Only one user can be assigned to an exclusive role.
-- 2. Exclusive role names must follow the format '{username}:exclusive'.
-- 3. The role description must be 'Exclusive role for user {username}.'.
create or replace function enforce_exclusive_role()
returns trigger as $$
declare
    associated_username text;
    expected_role_name text;
    expected_role_description text;
begin
    -- Check if the role is exclusive
    if exists (select 1 from roles where id = new.role_id and exclusive = true) then
        -- ensure only one user is assigned to the exclusive role
        if exists (
            select 1
            from role_users ru
            where ru.role_id = new.role_id
            and ru.user_id != new.user_id
        ) then
            raise exception 'exclusive role % can only have one user', new.role_id;
        end if;
        -- Get the username of the associated user
        select u.username into associated_username
        from users u
        where u.id = new.user_id;
        if associated_username is null then
            raise exception 'no valid user found for user_id %', new.user_id;
        end if;
        -- Construct the expected role name and description
        expected_role_name := associated_username || ':exclusive';
        expected_role_description := 'Exclusive role for user ' || associated_username || '.';
        -- Check if the role_name and role_description match the expected format
        if not exists (
            select 1
            from roles r
            where r.id = new.role_id
            and r.role_name = expected_role_name
            and r.role_description = expected_role_description
            and r.exclusive = true
        ) then
            raise exception 'Exclusive role % must have role_name ''%'' and role_description ''%''', 
                new.role_id, expected_role_name, expected_role_description;
        end if;
    end if;
    return new;
end;
$$ language plpgsql;

-- Trigger to enforce exclusive role constraints on role_users insert or update
create trigger enforce_exclusive_role_trigger
before insert or update on role_users
for each row
execute function enforce_exclusive_role();

--
-- insert users
--

insert into users (username, email, id, updated_by, password_hash) values
    (
        'system',                               -- username
        'no-email',                             -- email
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5', -- id
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5', -- updated_by
        'a5ad2e9965d47e970d4d6b7e123dbdb5'      -- password_hash
    );

--    (
--        'admin',                                -- username
--        'admin@example.com',                    -- email
--        'a546c200-12f6-47af-9652-3ac5c3d57f39', -- id
--        '0ed9f765-50ee-4e93-a711-f63db14b3cf5', -- updated_by
--        'af271d93c4b10bfe43269da6395a6b63'      -- password_hash
--    ),
--    (
--        'macnod',                               -- username
--        'macnod@example.com',                   -- email
--        '77988608-31cb-493b-ad06-af3a3db6f759', -- id
--        '0ed9f765-50ee-4e93-a711-f63db14b3cf5', -- updated_by
--        '564580ac65e798bfc5ad1bfbbdc8b78b'      -- password_hash
--    );
--

--
-- insert roles
--

insert into roles (role_name, role_description, exclusive, id, updated_by) values
    -- System role
    (
        'system',
        'Role for system operations. This role is not assigned to any user.',
        false,
        '200f91db-d0c8-43d1-9430-72f1fd03b285',
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
    ),

--    -- General roles
--    (
--        'admin',
--        'Role for admin operations. God-like role, initially assigned to the admin user.',
--        false,
--        '39fcd813-22a3-42ad-b489-2aa0a0a76d91',
--        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
--    ),
--    (
--        'editor',
--        'Role for makeing changes to resources.',
--        false,
--        '8f5b72d3-5008-420a-b0c5-31110697fcd4',
--        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
--    ),
--    (
--        'viewer',
--        'Role for viewing resources.',
--        false,
--        'aaa2b2fa-411b-4d71-b24a-600e4076d25b',
--        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
--    ),
    (
        'logged-in',
        'Role for logged-in users.',
        false,
        '9ad10cfa-78fe-43af-a417-0f104b64766a',
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
    ),
    (
        'guest',
        'Role for anonymous users that are not logged in.',
        false,
        'c9b6868a-0550-41eb-9c74-4d90e3ea03b5',
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
    ),
    -- Exclusive roles. 
    (
        'system:exclusive',
        'Exclusive role for user system.',
        true,
        '5afef606-07f8-4c7d-afe3-da66f00ed015',
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
    );
--    (
--        'logged-in:exclusive',
--        'Exclusive role for user admin.'
--        true,
--        '89ec7cdc-bb29-4276-8fd0-29acfa8fb08d',
--        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
--    ),
--    (
--        'macnod:exclusive',
--        'Exclusive role for user macnod.',
--        true,
--        '65555bf9-c80e-47b1-abcf-df74ba1eb5db',
--        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
--    );


--
-- insert permissions
--

insert into permissions (permission_name, id, updated_by) values
    (
        'create',
        '1d8f1218-7518-4c17-80f6-edb9987e8a20',
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
    ),
    (
        'read',
        '784c7460-134e-4b06-9c80-d6db197a1bdf',
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
    ),
    (
        'update',
        '8563f298-df16-490d-aac1-92fea8dd196d',
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
    ),
    (
        'delete',
        'f77b0d67-330a-4b0d-8166-d73229547f5f',
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
    );

--
-- insert resources
--

-- insert into resources (resource_name, resource_description, id, updated_by) values
--     (
--         '/admin/',
--         'Default resource for admin role.',
--         '7a170867-5a95-41ca-ad0f-edc9af80dc5c',
--         '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
--     ),
--     (
--         '/public/',
--         'Default resource for public role.',
--         'd8ccd5a1-eb0a-4cba-b854-9dd1072db137',
--         '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
--     ),
--     (
--         '/user/',
--         'Default resource for user role.',
--         'e618d348-8b69-4dc8-8d20-12d32415231f',
--         '0ed9f765-50ee-4e93-a711-f63db14b3cf5'
--     );


--
-- insert role-permissions
--

insert into role_permissions (role_id, permission_id, updated_by) values
    --
    -- General role permissions
    --
    -- system: create, read, update, delete
    (
        '200f91db-d0c8-43d1-9430-72f1fd03b285', -- system role
        '1d8f1218-7518-4c17-80f6-edb9987e8a20', -- create
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    ),
    (
        '200f91db-d0c8-43d1-9430-72f1fd03b285', -- system role
        '784c7460-134e-4b06-9c80-d6db197a1bdf', -- read
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    ),
    (
        '200f91db-d0c8-43d1-9430-72f1fd03b285', -- system role
        '8563f298-df16-490d-aac1-92fea8dd196d', -- update
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    ),
    (
        '200f91db-d0c8-43d1-9430-72f1fd03b285', -- system role
        'f77b0d67-330a-4b0d-8166-d73229547f5f', -- delete
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    ),
    -- -- admin: create, read, update, delete
    -- (
    --     '39fcd813-22a3-42ad-b489-2aa0a0a76d91', -- admin role
    --     '1d8f1218-7518-4c17-80f6-edb9987e8a20', -- create
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- (
    --     '39fcd813-22a3-42ad-b489-2aa0a0a76d91', -- admin role
    --     '784c7460-134e-4b06-9c80-d6db197a1bdf', -- read
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- (
    --     '39fcd813-22a3-42ad-b489-2aa0a0a76d91', -- admin role
    --     '8563f298-df16-490d-aac1-92fea8dd196d', -- update
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- (
    --     '39fcd813-22a3-42ad-b489-2aa0a0a76d91', -- admin role
    --     'f77b0d67-330a-4b0d-8166-d73229547f5f', -- delete
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- -- editor: read, update
    -- (
    --     '8f5b72d3-5008-420a-b0c5-31110697fcd4', -- editor role
    --     '784c7460-134e-4b06-9c80-d6db197a1bdf', -- read
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- (
    --     '8f5b72d3-5008-420a-b0c5-31110697fcd4', -- editor role
    --     '8563f298-df16-490d-aac1-92fea8dd196d', -- update
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- -- viewer: read
    -- (
    --     'aaa2b2fa-411b-4d71-b24a-600e4076d25b', -- viewer role
    --     '784c7460-134e-4b06-9c80-d6db197a1bdf', -- read
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    --
    -- Exclusive role permissions
    --
    -- system:exclusive
    (
        '5afef606-07f8-4c7d-afe3-da66f00ed015', -- system:exclusive role
        '1d8f1218-7518-4c17-80f6-edb9987e8a20', -- create
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    ),
    (
        '5afef606-07f8-4c7d-afe3-da66f00ed015', -- system:exclusive role
        '784c7460-134e-4b06-9c80-d6db197a1bdf', -- read
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    ),
    (
        '5afef606-07f8-4c7d-afe3-da66f00ed015', -- system:exclusive role
        '8563f298-df16-490d-aac1-92fea8dd196d', -- update
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    ),
    (
        '5afef606-07f8-4c7d-afe3-da66f00ed015', -- system:exclusive role
        'f77b0d67-330a-4b0d-8166-d73229547f5f', -- delete
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    );
    -- -- admin:exclusive
    -- (
    --     '89ec7cdc-bb29-4276-8fd0-29acfa8fb08d', -- admin:exclusive role
    --     '1d8f1218-7518-4c17-80f6-edb9987e8a20', -- create
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- (
    --     '89ec7cdc-bb29-4276-8fd0-29acfa8fb08d', -- admin:exclusive role
    --     '784c7460-134e-4b06-9c80-d6db197a1bdf', -- read
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- (
    --     '89ec7cdc-bb29-4276-8fd0-29acfa8fb08d', -- admin:exclusive role
    --     '8563f298-df16-490d-aac1-92fea8dd196d', -- update
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- (
    --     '89ec7cdc-bb29-4276-8fd0-29acfa8fb08d', -- admin:exclusive role
    --     'f77b0d67-330a-4b0d-8166-d73229547f5f', -- delete
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- -- macnod:exclusive
    -- (
    --     '65555bf9-c80e-47b1-abcf-df74ba1eb5db', -- macnod:exclusive role
    --     '784c7460-134e-4b06-9c80-d6db197a1bdf', -- read
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- (
    --     '65555bf9-c80e-47b1-abcf-df74ba1eb5db', -- macnod:exclusive role
    --     '8563f298-df16-490d-aac1-92fea8dd196d', -- update
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- );


--
-- insert role users
--

insert into role_users (role_id, user_id, updated_by) values
    -- System role
    (
        '200f91db-d0c8-43d1-9430-72f1fd03b285', -- system role
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5', -- system user
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    ),
    -- General roles
    -- -- admin user
    -- (
    --     '39fcd813-22a3-42ad-b489-2aa0a0a76d91', -- admin role
    --     'a546c200-12f6-47af-9652-3ac5c3d57f39', -- admin user
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- -- Editor role
    -- -- admin and macnod users
    -- (
    --     '8f5b72d3-5008-420a-b0c5-31110697fcd4', -- editor role
    --     'a546c200-12f6-47af-9652-3ac5c3d57f39', -- admin user
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- (
    --     '8f5b72d3-5008-420a-b0c5-31110697fcd4', -- editor role
    --     '77988608-31cb-493b-ad06-af3a3db6f759', -- macnod user
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- Guest role
    -- All users are assigned to the guest role
    -- (
    --     'c9b6868a-0550-41eb-9c74-4d90e3ea03b5', -- guest role
    --     'a546c200-12f6-47af-9652-3ac5c3d57f39', -- admin user
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- (
    --     'c9b6868a-0550-41eb-9c74-4d90e3ea03b5', -- guest role
    --     '77988608-31cb-493b-ad06-af3a3db6f759', -- macnod user
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- Exclusive roles
    -- Each exclusive role has only one user
    (
        '5afef606-07f8-4c7d-afe3-da66f00ed015', -- system:exclusive
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5', -- system user
        '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    );
    -- (
    --     '89ec7cdc-bb29-4276-8fd0-29acfa8fb08d', -- admin:exclusive
    --     'a546c200-12f6-47af-9652-3ac5c3d57f39', -- admin user
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- ),
    -- (
    --     '65555bf9-c80e-47b1-abcf-df74ba1eb5db', -- macnod:exclusive
    --     '77988608-31cb-493b-ad06-af3a3db6f759', -- macnod user
    --     '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system user
    -- );


--
-- insert resource roles
--

-- insert into resource_roles (resource_id, role_id, updated_by) values
--     (
--         '7a170867-5a95-41ca-ad0f-edc9af80dc5c', -- /admin/ resource
--         '39fcd813-22a3-42ad-b489-2aa0a0a76d91', -- admin role
--         '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system
--     ),
--     (
--         'd8ccd5a1-eb0a-4cba-b854-9dd1072db137', -- /public/ resource
--         'c9b6868a-0550-41eb-9c74-4d90e3ea03b5', -- guest trole
--         '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system
--     ),
--     (
--         'e618d348-8b69-4dc8-8d20-12d32415231f', -- /user/ resource
--         '39fcd813-22a3-42ad-b489-2aa0a0a76d91', -- admin role
--         '0ed9f765-50ee-4e93-a711-f63db14b3cf5'  -- system
--     );
