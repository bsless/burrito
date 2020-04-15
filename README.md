# Burrito

A Clojure utility tool for java interop interactive development.

## Usage

Add to your dev profile dependencies:

```clojure
[bsless/burrito "0.0.0-SNAPSHOT"]
```

### Wrap all methods

```clojure
(binding [*print-meta* true
          *trim-class-type* true
          *max-var-name-length* 1
          *trim-return-types* true
          *printable-primitive-types* true]
  (doseq [e (wrap java.util.concurrent.ConcurrentLinkedDeque)]
    (clojure.pprint/pprint e)))
```

### Wrap constructor

```clojure
(-> java.util.HashMap
    wrapping-candidates
    collapse-candidates
    (with-constructor java.util.HashMap)
    emit-constructor)
```

### Generate a list of all imports

```clojure
(imports java.util.HashMap)
```

## License

Copyright © 2020 Ben Sless

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
