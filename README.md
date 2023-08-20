# utf

Library of benchmarks pertaining to haskell/text#535.

## Explanations

Algorithms:

- *Attoparsec Hoehrmann* is a simple character-by-character parser that uses
  the decoder from `Data.Text.Internal.Encoding.Utf8` for validation;

- *Attoparsec Lazy* is the same style of parsing as the one above, but using
  `Codec.Encoding.UTF8` that this library provides;

- *Text Hoerhmann* is `decodeUtf8With` from `Data.Text.Lazy.Encoding`;

- *Text Lazy* is a new text parser made using `Codec.Encoding.UTF8`.

Benchmark types:

- *Correct* is valid UTF-8;

- *Early errors* is valid UTF-8 with a single `0xFF` character present
  at the start of every chunk;

- *Late errors* is valid UTF-8 with a single `0xFF` character present
  near the end of every chunk;

- *Garbage* is system noise.

All benchmarks consume data chunked at 4096 bytes, data is stored in RAM.

## Benchmarks

Following benchmarks were conducted using AMD Ryzen 5 3500U CPU.

<table>
  <thead>
    <tr>
      <th rowspan="3">Foundation</th>
      <th rowspan="3">Variant</th>
      <th colspan="8">Benchmark</th>
    </tr>
    <tr>
      <th colspan="2">Correct</th>
      <th colspan="2">Early errors</th>
      <th colspan="2">Late errors</th>
      <th colspan="2">Garbage</th>
    </tr>
    <tr>
      <th>32KiB</th>
      <th>2MiB</th>
      <th>32KiB</th>
      <th>2MiB</th>
      <th>32KiB</th>
      <th>2MiB</th>
      <th>32KiB</th>
      <th>2MiB</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td rowspan="2">Attoparsec</td>
      <td>Hoehrmann</td>
      <td>1.510 ms</td>
      <td>161.5 ms</td>
      <td>1.748 ms</td>
      <td>165.5 ms</td>
      <td>1.768 ms</td>
      <td>166.8 ms</td>
      <td>2.826 ms</td>
      <td>292.1 ms</td>
    </tr>
    <tr>
      <td>Lazy</td>
      <td>1.401 ms</td>
      <td>158.0 ms</td>
      <td>1.662 ms</td>
      <td>162.7 ms</td>
      <td>1.644 ms</td>
      <td>158.0 ms</td>
      <td>2.353 ms</td>
      <td>277.3 ms</td>
    </tr>
    <tr>
      <td rowspan="3">Text</td>
      <td>Hoehrmann (SIMD) </td>
      <td>13.70 μs</td>
      <td>1.124 ms</td>
      <td>35.46 μs</td>
      <td>1.687 ms</td>
      <td>167.6 μs</td>
      <td>12.57 ms</td>
      <td>7.156 ms</td>
      <td>463.0 ms</td>
    </tr>
    <tr>
      <td>Lazy (SIMD)</td>
      <td>10.68 μs</td>
      <td>833.5 μs</td>
      <td>16.81 μs</td>
      <td>1.236 ms</td>
      <td>103.1 μs</td>
      <td>7.905 ms</td>
      <td>3.494 ms</td>
      <td>223.4 ms</td>
    </tr>
    <tr>
      <td>Lazy</td>
      <td>94.16 μs</td>
      <td>7.614 ms</td>
      <td>117.9 μs</td>
      <td>8.528 ms</td>
      <td>118.8 μs</td>
      <td>8.575 ms</td>
      <td>606.9 μs</td>
      <td>41.23 ms</td>
    </tr>
  </tbody>
</table>
