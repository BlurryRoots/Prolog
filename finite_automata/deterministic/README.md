## Deterministic Finite Automaton

[DFA](https://en.wikipedia.org/wiki/Deterministic_finite_automaton)

### Declaration (5-tuple)
* Finite set of input characters (alphabet)
* Finite set of states
* Transition function delta
* Starting state
* Accepting states

<table>
  <tr>
    <th>Predicate</th><th>Description</th>
  </tr>
  <tr>
    <td>dfa_declare/5</td><td>Declares a dfa according to given 5-tuple</td>
  </tr>
</table>

### Getters
<table>
  <tr>
    <th>Predicate</th><th>Description</th>
  </tr>
  <tr>
    <td>dfa_get_states/1</td><td>Get states</td>
  </tr>
  <tr>
    <td>dfa_get_alphabet/1</td><td>Get input alphabet</td>
  </tr>
  <tr>
    <td>dfa_get_accepting_states/1</td><td>Get accepting states</td>
  </tr>
  <tr>
    <td>dfa_get_transitions/1</td><td>Get transitions</td>
  </tr>
  <tr>
    <td>dfa_get_starting_state/1</td><td>Get starting state</td>
  </tr>
</table>

### Processing
<table>
  <tr>
    <th>Predicate</th><th>Description</th>
  </tr>
  <tr>
    <td>dfa_delta/3</td><td>Call to get new state when given a start state and either a character or a word</td>
  </tr>
  <tr>
    <td>dfa_is_accepting_state/1</td><td>Check if given state is an accepting state</td>
  </tr>
</table>
