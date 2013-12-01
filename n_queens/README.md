## N Queens Problem
[Read about it here!](https://en.wikipedia.org/wiki/Eight_queens_puzzle)

### Disclaimer

The algorithm powering this solution is unreasonably slow. So be prepared to
wait quite a while, the get all solutions for anything where n > 9.

### Main functionality

<table>
  <tr>
    <th>Predicate</th><th>Description</th>
  </tr>
  <tr>
    <td>n_queens/2</td><td>Search for one solution to a given number of queens</td>
  </tr>
  <tr>
    <td>n_queens_list/2</td><td>Search for all solutions to a given number of queens</td>
  </tr>
</table>

### Acknowledgement

Thanks to J.R.Fisher, for his tutorial on the subject matter. It was a major inspiration for the my solution, i'm presenting here.
You can find it [here](http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_11.html).

### Future plans

Maybe i find time to get the algorithm up a notch, and strip of a good chunk of processing time, uneccessarily checking the same thing over and over again ;)
