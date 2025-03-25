%% KB

%% terras_tot_boom(+Boom, +Lengte)
terras_tot_boom(b1, 1).
terras_tot_boom(b2, 2).
terras_tot_boom(b3, 3).
terras_tot_boom(b4, 4).
terras_tot_boom(b5, 5).

%% boom_tot_boom(+Boom1, +Boom2, ?Lengte)
boom_tot_boom(b1, b2, 3).
boom_tot_boom(b1, b3, 4).
boom_tot_boom(b2, b3, 1).
boom_tot_boom(b3, b4, 4).
boom_tot_boom(b4, b5, 3).

% -------------------------------
% |     |  |            |       |
% b1 -- |  |            |       |
% |     b2 |            |       |
% |-------b3            |       |
%           ------------b4      |
%                        -------b5



bomen(L) :- findall(B,terras_tot_boom(B,_),L).