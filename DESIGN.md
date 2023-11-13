## Questions

ValueChoiceMap matters... but don't we need a ValueTrace, so get_choices can
return get_choice? isn't that the actual important idea?

- [ ] TODO take these implementation details from GEorge https://github.com/probcomp/Gen.jl/pull/274/files#diff-fff0e0ac690d9212d5bd4cb70ee308521d63ced3dbe0349a6c87d20438ea4ccbR90

- [ ] implement George's change to the choicemap interface, so `get_value` and
      `has_value` work on anything as a default.

- [ ] put the map-vs-Call distinction back into the dynamic trace, so I can have
      nested addresses.

- [ ] ask george about projection, addresses... how we should handle project on a valuechoicemap?
