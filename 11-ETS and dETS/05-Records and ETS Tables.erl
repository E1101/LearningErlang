rd(capital, {name, country, pop}).

ets:new(countries, [named_table, {keypos, #capital.name}]).

ets:insert(countries, #capital{name="Budapest", country="Hungary", pop=2400000}).
ets:insert(countries, #capital{name="Pretoria", country="South Africa", pop=2400000}).
ets:insert(countries, #capital{name="Rome", country="Italy", pop=5500000}).

ets:lookup(countries, "Pretoria").
%% [#capital{name = "Pretoria",country = "South Africa", pop = 2400000}]

ets:match(countries, #capital{name='$1',country='$2', _='_'}).
%% [["Rome","Italy"],
%% ["Budapest","Hungary"],
%% ["Pretoria","South Africa"]]

ets:match_object(countries, #capital{country="Italy", _='_'}).
%%[#capital{name = "Rome",country = "Italy", pop = 5500000}]

MS = ets:fun2ms(fun(#capital{pop=P, name=N}) when P < 5000000 -> N end).
%% [{#capital{name = '$1',country = '_',pop = '$2'},
%% [{'<','$2',5000000}],
%% ['$1']}]
ets:select(countries, MS).
%% ["Budapest","Pretoria"]

