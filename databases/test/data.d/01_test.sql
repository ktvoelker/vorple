
INSERT INTO users (password, email)
VALUES (CRYPT('foobar', GEN_SALT('bf')), 'ktvoelker@gmail.com');

