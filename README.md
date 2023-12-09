# CS3110 Final Project

| Name          | NetID  |
| ------------- | ------ |
| Andrew Lin    | al2245 |
| Jason Dong    | jd876  |
| Dalton Luce   | dcl252 |
| Su Yean Leong | sl2658 |

## Environment

1. Create a new switch named "final_project"

    ```text
    opam switch create final_project 5.0.0
    ```

2. Install our required packages

    ```text
    opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat ocamlformat-rpc yojson bogue
    ```

3. Ensure you are on the correct switch

    ```text
    opam switch list
    ```

    It should look something like:

    ![switch](assets/switch.png)

## Running

* Command line: `make yum`
* GUI: `make yummy`

## Checking LOC

1. Install cloc using homebrew (install homebrew if neccesary)

    ```text
    brew install cloc
    ```

2. Run command in source directory

    ```text
    cloc --by-file --include-lang=OCaml .
    ```
    
## [Demo Plan](https://docs.google.com/document/d/1-ZhjeN8IY0uvJLSqNvl0T1y04bVGAgijMMVdawAWykw/edit?usp=sharing)

## [MS3 Report](https://docs.google.com/document/d/1Io5gyc-MZLV1laxnKsWnB233hYNzaCGRQgO3Iz9oB5M/edit?usp=sharing)

## [Team Contract](/CONTRACT.md)

## [Roadmap](/ROADMAP.md)

## [Trello](https://trello.com/b/iJHJ0XU2/yummy-ms2)
