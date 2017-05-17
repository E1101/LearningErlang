%% defines a record type named planemo , containing fields named name , gravity , and
%% distance_from_sun . Right now, when you create a new record, the fields will all have
%% the value undefined.

-record(planemo, {name, gravity, diameter, distance_from_sun}).

%% With default values
-record(tower, {location, height=20, planemo=earth, name}).


% > To share record declarations reliably, just put the record declarations in their
% - own file, ending with the extension .hrl
% > records.hrl file containing two rather unrelated record declarations
-record(planemo, {name, gravity, diameter, distance_from_sun}).
-record(tower, {location, height=20, planemo=earth, name}).


%% # ############################ #
%% # Creating and Reading Records #
%% # ############################ #

Tower1=#tower{}. % #tower{location = undefined,height = 20,planemo = earth, name = undefined}
Tower4=#tower{location="Rupes Altai 241", height=500, planemo=moon, name="Piccolomini View"}.
Tower5=#tower{planemo=mars, height=500, name="Daga Vallis", location="Valles Marineris"}.

Tower5#tower.planemo. % mars
#tower{location=L5, height=H5} = Tower5.
L5. % "Valles Marineris"
H5. % 500

Tower5a=Tower5#tower{height=512}.
% #tower{location = "Valles Marineris",height = 512, planemo = mars,name = "Daga Vallis"}
