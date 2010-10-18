create table entries (
  id serial primary key,
  content text not null
);

create table comments (
  id serial primary key,
  entry_id int not null references entries,
  content text not null,
  username text not null
);