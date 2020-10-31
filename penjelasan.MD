# Penjelasan Tugas 5
## Nama : Glenda Emanuella Sutanto
## NPM  : 1806133774

### Modifikasi
Modifikasi yang saya lakukan terdapat pada file `src/Parser.hs` dan `src/Reducer/Renderer.hs`. 
- File `src/Parser.hs` mengurus segala urusan tentang parsing, yaitu ketika setelah kita menerima input dan sebelum input tersebut diproses oleh `src/Reducer.hs` untuk dilakukan reduksi berdasarkan lambda calculus. 
- File `src/Reducer/Renderer.hs` mengurus segala urusan tentang rendering, yaitu sebelum output ditampilkan ke layar.

#### `src/Parser.hs`
Untuk file ini, saya lakukan perubahan agar bisa mengubah bilangan atau ekspresi penjumlahan dan perkalian ke representasi lambda calculus nya sesuai <b><i>Church's numeral</i></b>. 
- Bilangan cacah 1 digit yaitu 0 sampai 9, menjadi `(\s.(\z.z))`, `(\s.(\z.(s z)))`, dst.
- Operasi penjumlahan, menjadi `(\w.(\y.(\x.(y(w y x)))))`
- Operasi perkalian, menjadi `(\x.(\y.(\z.(x(y z)))))`

Di sini saya melakukan modifikasi untuk meng-handle beberapa kasus, yaitu:
- Input merupakan bilangan cacah 0-9, maka akan di-outputkan representasi lambda calculus nya
- Input merupakan ekspresi penjumlahan a+b+c+... dst.
- Apabila diberikan input ekspresi yang mengandung operasi perkalian (perlu diingat bahwa pada lambda calculus perkalian diaplikasikan secara prefix):
    - Jika operasi perkalian di-inputkan secara prefix, maka masalah tersebut sudah di-handle. Lambda interpreter dapat berfungsi sebagaimana mestinya. Contoh: `*5((*23)+3)` 
    - Jika operasi perkalian di-inputkan secara infix dan tidak mengandung karakter `(` atau `)`, maka:
        - Input harus hanya merupakan operasi perkalian (tidak ada operasi penjumlahan), contoh: `1*2*3*4`; atau
        - Input merupakan kombinasi perkalian dan penjumlahan, namun perkalian dibatasi tidak boleh terdiri dari lebih dari dua bilangan berturut-turut, contoh: `1*2+3+4+5*2`

Kasus yang belum sempat di-handle:
- Apabila input merupakan kombinasi perkalian infix dan penjumlahan, tidak mengandung karakter `(` maupun `)`, tetapi operasi perkalian bisa terdiri dari lebih dua bilangan berturut-turut, contoh: `1+2*3*4+5`
- Apabila input merupakan kombinasi perkalian infix dan penjumlahan dan terdapat tanda kurung sehingga <i>precedence</i> yang paling tinggi adalah yang di dalam tanda kurung


#### `src/Reducer/Renderer.hs`
Untuk file ini, saya lakukan perubahan agar bisa mengkonversikan representasi bilangan dalam <b><i>Church's numeral</i></b> kedalam bentuk angka sehingga angka tersebut bisa di-output-kan ke layar. Contoh: `(\s.(\z.z))` menjadi `0`.

Ide saya adalah, memeriksa term terakhir yang di-outputkan, apakah ia berbentuk representasi bilangan dalam  <b><i>Church's numeral</i></b> atau tidak. Idenya dengan memanfaatkan typeclass `LambdaTerm` pada program ini.

Referensi source code: ([Lihat disini](https://stackoverflow.com/questions/58687538/converting-from-church-encoding-to-numerals))