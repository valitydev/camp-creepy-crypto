# Creepy Crypto

Экстремально короткий курс по основам и инструментам современной криптографии.

## Установка

Для работы с лекциями нужен [Backslide](https://github.com/sinedied/backslide).

Для работы со слайдами в браузере:

```
bs serve
```

## План лекций

### Лекция 1

#### Теория

- [01-01-symmetric.md](01-01-symmetric.md)

> ≈ 75 минут

О чём: задачи, теоретические инструменты, симметричное шифрование, контроль целостности

* one time pad
    - конфиденциальность
    - грааль perfect security
    - размер ключа ≥ размер сообщения
* stream ciphers
    - csprngs
    - semantic security
        - вероятностная модель
        - игра _отличить одно от другого_
        - практически допустимые границы вероятностей
    - игра _отличи случайный поток бит от выхлопа алгоритма со случайным сидом_
    - coa
    - не более одного сообщения с одним ключом
* block ciphers
    - блоки определённого размера
    - prp
    - более _сильные_
    - ecb и semantic security
    - ~~prf~~
    - det ctr / аналогия с stream cipher
    - cpa / probabilistic algos / randomization
    - cbc / iv / ~~rand ctr~~
    - malleability + cca

#### Практика

> ≈ 240 минут

1. Получение plaintext с помощью CCA на [сервер](pador/), использующий AES-CBC с padding oracle.


## Лекция 2

- [01-02-integrity.md](01-02-integrity.md)

#### Теория

> ≈ 45 минут

О чём: контроль целостности

* message integrity
    - плюс authenticity
    - не confidentiality!
    - нужен ключ, иначе нет authenticity
    - игра _попробуй подделать подпись_
    - AES в CBC-MAC
    - хэши как PRF
        - one way
        - collision resistance
    - HMAC

#### Практика

> ≈ 240 минут

1. Изменение статуса пользователя с помощью length extension attack на доморощенный алгоритм подписи вида `H(secret || message)`, используемый в [чучельном сервисе](lenex/).


## Лекция 3

- [01-03-authenticated-encryption.md](01-03-authenticated-encryption.md)

> ≈ 45 минут

#### Теория

О чём: аутентифицированное шифрование

* схема шифрования с bottom
    - confidentiality + integrity
    - можем не расшифровать невалидный ct
    - security under cca
        + пример с перехватом почты
        + равно cpa security + ciphertext integrity
        + что такое ciphertext integrity
            + _попробуй сформировать валидный ct_
    - скомбинировать cpa secure cipher + unforgeable mac
        + mac-then-encrypt
        + encrypt-then-mac
        + encrypt-and-mac
    - сформулировать схему aead
        + authentcated data

#### Практика

> ≈ Много минут

1. Восстановление части сообщения у [чучела SSHv2 сервера](nossh/), уязвимого засчёт non-atomic decryption.


## Лекция 4

- [02-01-number-theory.md](02-01-number-theory.md)
- [02-02-asymmetric.md](02-02-asymmetric.md)

> ≈ 75 минут

О чём: сложность проблем, обмен ключами, асимметричное шифрование

#### Теория

* number theory
    - арифметика остатков
    - обратный элемент
    - генераторы и циклические группы
    - порядок группы
* асимметрия
    - зачем?
    - пара ключей
    - любой может сформировать ct
* схема асимметричного шифрования
    - trapdoor one-way functions
    - rsa
    - сложность factors(n)
    - rsa + hash + cca cipher
* задача обмена ключами
    - trusted party – неудобно
    - наивный diffie-hellman
    - сложность dlog_g
    - надёжность схемы
    - нет authenticity
    - elgamal
* кратко цифровых подписях
    - rsa
    - pki

#### Практика

> ≈ 240 минут

1. Восстановление сообщения у [чучела TLS сервера](bleach/), уязвимого к Baby Bleichenbacher attack.
