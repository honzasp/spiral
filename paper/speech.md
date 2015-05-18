# Spiral
- (příprava)
  - vytištěnou tuto osnovu na stole
  - sednout si tak, ať dobře vidím na plátno
  - nastavit písmo, ať jde dobře vidět
  - `cd ~/prog/spiral`
  - zkontrolovat, že v `samples` je jen `hello.spiral` a `factorial.spiral`
  - terminálové okno, může být úzké
  - Vim, ať se vejdou tři soubory pod sebe
  - `:set nowrap`, horizontální scroll `zH` a `zL`, `zs` a `ze`
  - v obou oknech anglická klávesnice
  - papírová práce po ruce

- vážená poroto, profesoři (spolužáci)
- jednoduchý programovací jazyk
- funkcionální, dynamicky typovaný
- několik drobných programů, ukážu vlastnosti jazyka a průběh překladu

## Příklady
### Hello world!
- začneme samozřejmě programem "Hello world!"
- Vim: `samples/hello.spiral`
  - import modulu `std`, což je standardní knihovna jazyka Spiral
  - výpis na standardní výstup
- překladač se překvapivě jmenuje "spiral" a ovládá se z příkazové řádky

    $ ./target/debug/spiral --help

- musíme předat cestu ke složce, kde má překladač hledat importované moduly, v
  našem případě modul `std` s funkcí `println`
- dále je třeba specifikovat cestu k přeložené běhové knihovně

    $ ./target/debug/spiral -I stdlib/ -t rt/build/runtime_debug.a \
      samples/hello.spiral
    $ ./samples/hello

### Faktoriál
- druhý, podobně jednoduchý příklad
- Vim: `samples/factorial.spiral` (`:e`)
- moduly `std.io` a `std.string`
- funkce `factorial`, standardní rekurzivní definice
- prefixová notace, v jazyku nejsou žádné operátory, všechno jsou funkce
- načtení čísla ze standardního vstupu, výstup na standardní výstup 
  - `stringify` převede číslo (nebo cokoli jiného) na řetězec, protože
    `io-write-line` vypisuje pouze řetězce

    $ ./target/debug/spiral -I stdlib/ -t rt/build/runtime_debug.a \
      samples/factorial.spiral
    $ echo "5" | ./samples/factorial

## Postup překladu
### Spine
- program ze Spiral je nejprve přeložen do mezijazyka Spine
- překladač mi umožňuje vypsat reprezentaci v jazyku Spine, takže vám ji můžu
  ukázat

    $ ./target/debug/spiral -I stdlib -e spine samples/factorial.spiral

- (nemusím dávat runtime, protože program se nelinkuje.)
- Vim: `samples/factorial.spiral`, `samples/factorial.spine` (`:split`),
- vidíme definice funkcí ze standardní knihovny
- Vim: ve `factorial.spine` najít `factorial`, `z<CR>`, `zs`
- základním stavebním kamenem je continuation, což je speciální funkce, která
  nikdy nevrátí, její volání je tedy vlastně skok
- ukázat, že nejprve se volá `<=`, skok do `join_46`, podle výsledku skok do
  `branch-then_18` nebo `branch-else_18`, v "then" vrátí `1`, v "else" zavolá
  `-` a znovu zavolá `factorial`

### Grit
- další překlad je ze Spine do Gritu

    $ ./target/debug/spiral -I stdlib -e grit samples/factorial.spiral
  
- Vim: `samples/factorial.spiral`, `samples/factorial.grit`
- imperativní posloupnost příkazů
- funkce `factorial_1_1`, začíná na start, volá `spiral_std_le` (implementace
  menší než), skáče... proskákat celou funkcí
- mnoho skoků, protože program prošel několika optimalizacemi

### Assembler
- konečný překlad do assembleru (gas je GNU Assembler)

    $ ./target/debug/spiral -I stdlib -e gas samples/factorial.spiral

- Vim: `samples/factorial.spiral`, `samples/factorial.s`
- funkce faktoriál
- jen přesouvání hodnot z místa na místo, podmíněné skoky a volání funkcí

## Runtime
### Ukázka primitivní funkce
- všechny primitivní funkce jsou implementované v běhové knihovně
- ukážu funkci `spiral_std_io_write_line`
- Vim `rt/src/spiral/io.cpp`, `samples/factorial.s`,
  `samples/factorial.spiral`
  - najít `write_line`
- rozbalí si IO objekt a řetězec a pomocí céčkovské knihovny jej vypíše
- v běhové knihovně toho je mnohem více, garbage collector, všechny ostatní
  funkce, ...

### Dotazy
- vše co jsem chtěl ukázat, dotazy?
