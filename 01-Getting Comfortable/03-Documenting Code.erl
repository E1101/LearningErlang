%% # ################ #
%% # Documenting Code #
%% # ################ #

%%% < means that the comment will be formatted flush left
%%  < means the comment is indented with surrounding code
%   < is used for comments on the end of a line


%% # ################ #
%% #      EDoc        #
%% # ################ #

% Erlang includes a documentation system called EDoc, which converts comments placed
% in the code into navigable HTML documentation. It relies on specially formatted com‐
% ments, a directive, and occasionally an extra file to provide structured information about
% your modules and application.

% > Erlang can build the files for you using the EDoc file function:
edoc:files(["drop.erl"], [{dir, "doc"}]).


%% # ################### #
%% # Documenting Modules #
%% # ################### #

%% @author Simon St.Laurent <simonstl@simonstl.com> [http://simonstl.com]
%% @doc Functions calculating velocities achieved by objects
%% dropped in a vacuum.
%% @reference from <a href= "http://shop.oreilly.com/product/0636920025818.do" >In troducing Erlang</a>,
%% O'Reilly Media, Inc., 2012.
%% @copyright 2012 by Simon St.Laurent
%% @version 0.1
%% @todo Todo ....

-module(drop).
-export([fall_velocity/1]).
fall_velocity(Distance) -> math:sqrt(2 * 9.8 * Distance).


%% # ##################### #
%% # Documenting Functions #
%% # ##################### #

%% @doc Calculates the velocity of an object falling on Earth
%% as if it were in a vacuum (no air resistance). The distance is
%% the height from which the object falls, specified in meters,
%% and the function returns a velocity in meters per second.

% ! EDoc will combine the types specified in the -spec directive with the parameter names
% !-in the actual function
-spec(fall_velocity(number()) -> number()).
fall_velocity(Distance) -> math:sqrt(2 * 9.8 * Distance).


%% # ############################ #
%% # Documenting Your Application #
%% # ############################ #

% Sometimes you want information like the author and copyright data to appear in every
% module, often when it varies from module to module. Other times that becomes clutter,
% and it’s easier to put it into one place where it applies to all of your modules.

% > create an [overview.edoc] file in your project’s doc directory.
%   re-generate documentation and click on the Overview link.
% ! don’t need to preface every line with %%
@author Simon St.Laurent <simonstl@simonstl.com> [http://simonstl.com]
@doc Functions for calculating and converting velocities.
@reference from <a href= "http://shop.oreilly.com/product/0636920025818.do"
>Introducing Erlang</a>, O'Reilly Media, Inc., 2012.
@copyright 2012 by Simon St.Laurent
@version 0.1


%% # ############# #
%% # Generate EDoc #
%% # ############# #

edoc:files(["drop.erl", "convert.erl", "combined.erl"]).
