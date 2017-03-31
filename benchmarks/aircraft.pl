% CVS: $Id: aircraft.pl,v 1.2 1998/10/19 04:20:47 pets Exp $
:- unknown(_,fail).
:- multifile the_Aircraft_on_profile/2.
:- dynamic the_Aircraft_on_profile/2.
:- prolog_flag(compiling, _, fastcode).

:- op(100, xfx, is_north_of).
:- op(100, xfx, is_at_or_north_of).
:- op(100, xfx, is_south_of).
:- op(100, xfx, is_at_or_south_of).

:- op(100, xfx, is_east_of).
:- op(100, xfx, is_west_of).
:- op(100, xfx, is_at_or_west_of).

:- op(100, xfx, is_at_or_above).
:- op(100, xfx, is_at_or_below).
:- op(100, xfx, is_above).
:- op(100, xfx, is_below).

:- op(100, xfx, is_at_or_later_than).
:- op(100, xfx, is_at_or_earlier_than).
:- op(100, xfx, is_earlier_than).
:- op(100, xfx, is_later_than).

:- op(100, xfx, lies_in_flight_level_range_of).

:- op(100, xfx, is_during_time_period_of).
        
:- op(100, xfx, belongs_to).



not__(Goal):-
	\+(Goal).

% 'ground not' is a library predicate (not) in Quintus, but needs to
%be defined in 
% Sicstus 

%% MMW 23/03/98
%% however for current purposes it can be given the same def as not__

ground_not__(Goal):-

	\+(Goal).




the_Aircraft_on_profile(profile_AFR002_1,afr002).

the_Type_of(afr002,conc).

the_Segment(profile_AFR002_1,fourD_pt(threeD_pt(twoD_pt(lat_N(50,41),long_W(15,0)),fl_range(fl(450),fl(450))),time(11,13,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(50,50),long_W(20,0)),fl_range(fl(450),fl(450))),time(11,23,0)),2.0)belongs_to profile_AFR002_1.

the_Segment(profile_AFR002_1,fourD_pt(threeD_pt(twoD_pt(lat_N(50,50),long_W(20,0)),fl_range(fl(450),fl(450))),time(11,23,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(50,30),long_W(30,0)),fl_range(fl(450),fl(450))),time(11,44,0)),2.0)belongs_to profile_AFR002_1.

the_Segment(profile_AFR002_1,fourD_pt(threeD_pt(twoD_pt(lat_N(50,30),long_W(30,0)),fl_range(fl(450),fl(450))),time(11,44,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(49,16),long_W(40,0)),fl_range(fl(450),fl(450))),time(12,6,0)),2.0)belongs_to profile_AFR002_1.

the_Segment(profile_AFR002_1,fourD_pt(threeD_pt(twoD_pt(lat_N(49,16),long_W(40,0)),fl_range(fl(450),fl(450))),time(12,6,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(47,3),long_W(50,0)),fl_range(fl(600),fl(600))),time(12,30,0)),2.0)belongs_to profile_AFR002_1.

the_Aircraft_on_profile(profile_BAW1_1,baw1).

the_Type_of(baw1,conc).

the_Segment(profile_BAW1_1,fourD_pt(threeD_pt(twoD_pt(lat_N(50,41),long_W(15,0)),fl_range(fl(600),fl(600))),time(11,32,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(50,50),long_W(20,0)),fl_range(fl(600),fl(600))),time(11,42,0)),2.0)belongs_to profile_BAW1_1.

the_Segment(profile_BAW1_1,fourD_pt(threeD_pt(twoD_pt(lat_N(50,50),long_W(20,0)),fl_range(fl(600),fl(600))),time(11,42,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(50,30),long_W(30,0)),fl_range(fl(600),fl(600))),time(12,3,0)),2.0)belongs_to profile_BAW1_1.

the_Segment(profile_BAW1_1,fourD_pt(threeD_pt(twoD_pt(lat_N(50,30),long_W(30,0)),fl_range(fl(600),fl(600))),time(12,3,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(49,16),long_W(40,0)),fl_range(fl(600),fl(600))),time(12,25,0)),2.0)belongs_to profile_BAW1_1.

the_Segment(profile_BAW1_1,fourD_pt(threeD_pt(twoD_pt(lat_N(49,16),long_W(40,0)),fl_range(fl(600),fl(600))),time(12,25,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(47,3),long_W(50,0)),fl_range(fl(450),fl(450))),time(12,49,0)),2.0)belongs_to profile_BAW1_1.

the_Aircraft_on_profile(profile_BAW2_1,baw2).

the_Type_of(baw2,conc).

the_Segment(profile_BAW2_1,fourD_pt(threeD_pt(twoD_pt(lat_N(48,10),long_W(40,0)),fl_range(fl(590),fl(590))),time(15,25,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(49,26),long_W(30,0)),fl_range(fl(590),fl(590))),time(15,46,0)),2.0)belongs_to profile_BAW2_1.

the_Segment(profile_BAW2_1,fourD_pt(threeD_pt(twoD_pt(lat_N(49,26),long_W(30,0)),fl_range(fl(590),fl(590))),time(15,46,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(49,49),long_W(20,0)),fl_range(fl(600),fl(600))),time(16,6,0)),2.0)belongs_to profile_BAW2_1.

the_Segment(profile_BAW2_1,fourD_pt(threeD_pt(twoD_pt(lat_N(49,49),long_W(20,0)),fl_range(fl(600),fl(600))),time(16,6,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(49,41),long_W(15,0)),fl_range(fl(450),fl(450))),time(16,16,0)),2.0)belongs_to profile_BAW2_1.

the_Segment(profile_BAW2_1,fourD_pt(threeD_pt(twoD_pt(lat_N(49,41),long_W(15,0)),fl_range(fl(450),fl(450))),time(16,16,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(50,25),long_W(8,0)),fl_range(fl(450),fl(450))),time(16,30,0)),2.0)belongs_to profile_BAW2_1.

the_Aircraft_on_profile(profile_GBOAE_1,gboae).

the_Type_of(gboae,conc).

the_Segment(profile_GBOAE_1,fourD_pt(threeD_pt(twoD_pt(lat_N(48,10),long_W(40,0)),fl_range(fl(600),fl(600))),time(16,21,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(49,26),long_W(30,0)),fl_range(fl(600),fl(600))),time(16,41,0)),2.0)belongs_to profile_GBOAE_1.

the_Segment(profile_GBOAE_1,fourD_pt(threeD_pt(twoD_pt(lat_N(49,26),long_W(30,0)),fl_range(fl(600),fl(600))),time(16,41,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(49,49),long_W(20,0)),fl_range(fl(600),fl(600))),time(17,1,0)),2.0)belongs_to profile_GBOAE_1.

the_Segment(profile_GBOAE_1,fourD_pt(threeD_pt(twoD_pt(lat_N(49,49),long_W(20,0)),fl_range(fl(600),fl(600))),time(17,1,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(49,41),long_W(15,0)),fl_range(fl(450),fl(450))),time(17,11,0)),2.0)belongs_to profile_GBOAE_1.

the_Segment(profile_GBOAE_1,fourD_pt(threeD_pt(twoD_pt(lat_N(49,41),long_W(15,0)),fl_range(fl(450),fl(450))),time(17,11,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(50,25),long_W(8,0)),fl_range(fl(450),fl(450))),time(17,25,0)),2.0)belongs_to profile_GBOAE_1.

the_Aircraft_on_profile(profile_AFR001_1,afr001).

the_Type_of(afr001,conc).

the_Segment(profile_AFR001_1,fourD_pt(threeD_pt(twoD_pt(lat_N(48,10),long_W(40,0)),fl_range(fl(590),fl(590))),time(19,40,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(49,26),long_W(30,0)),fl_range(fl(590),fl(590))),time(20,0,0)),2.0)belongs_to profile_AFR001_1.

the_Segment(profile_AFR001_1,fourD_pt(threeD_pt(twoD_pt(lat_N(49,26),long_W(30,0)),fl_range(fl(590),fl(590))),time(20,0,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(49,49),long_W(20,0)),fl_range(fl(450),fl(450))),time(20,20,0)),2.0)belongs_to profile_AFR001_1.

the_Segment(profile_AFR001_1,fourD_pt(threeD_pt(twoD_pt(lat_N(49,49),long_W(20,0)),fl_range(fl(450),fl(450))),time(20,20,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(49,41),long_W(15,0)),fl_range(fl(450),fl(450))),time(20,29,0)),2.0)belongs_to profile_AFR001_1.

the_Segment(profile_AFR001_1,fourD_pt(threeD_pt(twoD_pt(lat_N(49,41),long_W(15,0)),fl_range(fl(450),fl(450))),time(20,29,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(49,6),long_W(8,0)),fl_range(fl(450),fl(450))),time(20,44,0)),2.0)belongs_to profile_AFR001_1.

the_Aircraft_on_profile(profile_BAW3_1,baw3).

the_Type_of(baw3,conc).

the_Segment(profile_BAW3_1,fourD_pt(threeD_pt(twoD_pt(lat_N(50,41),long_W(15,0)),fl_range(fl(600),fl(600))),time(20,3,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(50,50),long_W(20,0)),fl_range(fl(600),fl(600))),time(20,14,0)),2.0)belongs_to profile_BAW3_1.

the_Segment(profile_BAW3_1,fourD_pt(threeD_pt(twoD_pt(lat_N(50,50),long_W(20,0)),fl_range(fl(600),fl(600))),time(20,14,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(50,30),long_W(30,0)),fl_range(fl(600),fl(600))),time(20,35,0)),2.0)belongs_to profile_BAW3_1.

the_Segment(profile_BAW3_1,fourD_pt(threeD_pt(twoD_pt(lat_N(50,30),long_W(30,0)),fl_range(fl(600),fl(600))),time(20,35,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(49,16),long_W(40,0)),fl_range(fl(600),fl(600))),time(20,57,0)),2.0)belongs_to profile_BAW3_1.

the_Segment(profile_BAW3_1,fourD_pt(threeD_pt(twoD_pt(lat_N(49,16),long_W(40,0)),fl_range(fl(600),fl(600))),time(20,57,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(47,3),long_W(50,0)),fl_range(fl(450),fl(450))),time(21,21,0)),2.0)belongs_to profile_BAW3_1.

the_Aircraft_on_profile(profile_N623MS_1,n623ms).

the_Type_of(n623ms,g3).

the_Segment(profile_N623MS_1,fourD_pt(threeD_pt(twoD_pt(lat_N(50),long_W(40)),fl_range(fl(360),fl(360))),time(4,40,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(52),long_W(30)),fl_range(fl(360),fl(360))),time(5,23,0)),0.8)belongs_to profile_N623MS_1.

the_Segment(profile_N623MS_1,fourD_pt(threeD_pt(twoD_pt(lat_N(52),long_W(30)),fl_range(fl(360),fl(360))),time(5,23,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(53),long_W(20)),fl_range(fl(360),fl(360))),time(6,4,0)),0.8)belongs_to profile_N623MS_1.

the_Segment(profile_N623MS_1,fourD_pt(threeD_pt(twoD_pt(lat_N(53),long_W(20)),fl_range(fl(360),fl(360))),time(6,4,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(53),long_W(15)),fl_range(fl(360),fl(360))),time(6,25,0)),0.8)belongs_to profile_N623MS_1.

the_Aircraft_on_profile(profile_KLM734_1,klm734).

the_Type_of(klm734,b747).

the_Segment(profile_KLM734_1,fourD_pt(threeD_pt(twoD_pt(lat_N(44),long_W(40)),fl_range(fl(360),fl(360))),time(4,28,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(48),long_W(30)),fl_range(fl(380),fl(380))),time(5,18,0)),0.85)belongs_to profile_KLM734_1.

the_Segment(profile_KLM734_1,fourD_pt(threeD_pt(twoD_pt(lat_N(48),long_W(30)),fl_range(fl(380),fl(380))),time(5,18,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(49),long_W(20)),fl_range(fl(380),fl(380))),time(6,3,0)),0.85)belongs_to profile_KLM734_1.

the_Segment(profile_KLM734_1,fourD_pt(threeD_pt(twoD_pt(lat_N(49),long_W(20)),fl_range(fl(380),fl(380))),time(6,3,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(49),long_W(15)),fl_range(fl(380),fl(380))),time(6,25,0)),0.85)belongs_to profile_KLM734_1.

the_Aircraft_on_profile(profile_EIN108_1,ein108).

the_Type_of(ein108,b747).

the_Segment(profile_EIN108_1,fourD_pt(threeD_pt(twoD_pt(lat_N(50),long_W(40)),fl_range(fl(380),fl(380))),time(4,45,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(52),long_W(30)),fl_range(fl(360),fl(360))),time(5,28,0)),0.83)belongs_to profile_EIN108_1.

the_Segment(profile_EIN108_1,fourD_pt(threeD_pt(twoD_pt(lat_N(52),long_W(30)),fl_range(fl(360),fl(360))),time(5,28,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(53),long_W(20)),fl_range(fl(360),fl(360))),time(6,8,0)),0.83)belongs_to profile_EIN108_1.

the_Segment(profile_EIN108_1,fourD_pt(threeD_pt(twoD_pt(lat_N(53),long_W(20)),fl_range(fl(360),fl(360))),time(6,8,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(53),long_W(15)),fl_range(fl(380),fl(380))),time(6,28,0)),0.83)belongs_to profile_EIN108_1.

the_Aircraft_on_profile(profile_CRL927_1,crl927).

the_Type_of(crl927,b747).

the_Segment(profile_CRL927_1,fourD_pt(threeD_pt(twoD_pt(lat_N(42),long_W(40)),fl_range(fl(340),fl(340))),time(3,53,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(46),long_W(30)),fl_range(fl(340),fl(340))),time(4,47,0)),0.84)belongs_to profile_CRL927_1.

the_Segment(profile_CRL927_1,fourD_pt(threeD_pt(twoD_pt(lat_N(46),long_W(30)),fl_range(fl(340),fl(340))),time(4,47,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(48),long_W(20)),fl_range(fl(340),fl(340))),time(5,34,0)),0.84)belongs_to profile_CRL927_1.

the_Segment(profile_CRL927_1,fourD_pt(threeD_pt(twoD_pt(lat_N(48),long_W(20)),fl_range(fl(340),fl(340))),time(5,34,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(48),long_W(15)),fl_range(fl(340),fl(340))),time(5,58,0)),0.84)belongs_to profile_CRL927_1.

the_Segment(profile_CRL927_1,fourD_pt(threeD_pt(twoD_pt(lat_N(48),long_W(15)),fl_range(fl(340),fl(340))),time(5,58,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(48),long_W(8)),fl_range(fl(340),fl(340))),time(6,31,0)),0.84)belongs_to profile_CRL927_1.

the_Aircraft_on_profile(profile_GOLD11_1,gold11).

the_Type_of(gold11,c135).

the_Segment(profile_GOLD11_1,fourD_pt(threeD_pt(twoD_pt(lat_N(54),long_W(40)),fl_range(fl(210),fl(210))),time(7,36,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(55),long_W(30)),fl_range(fl(210),fl(210))),time(8,20,0)),0.8)belongs_to profile_GOLD11_1.

the_Segment(profile_GOLD11_1,fourD_pt(threeD_pt(twoD_pt(lat_N(55),long_W(30)),fl_range(fl(210),fl(210))),time(8,20,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(56),long_W(20)),fl_range(fl(210),fl(210))),time(9,2,0)),0.8)belongs_to profile_GOLD11_1.

the_Segment(profile_GOLD11_1,fourD_pt(threeD_pt(twoD_pt(lat_N(56),long_W(20)),fl_range(fl(210),fl(210))),time(9,2,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(56),long_W(10)),fl_range(fl(230),fl(230))),time(9,39,0)),0.8)belongs_to profile_GOLD11_1.

the_Aircraft_on_profile(profile_CUBE51_1,cube51).

the_Type_of(cube51,f111).

meets_mnps(cube51).

the_Segment(profile_CUBE51_1,fourD_pt(threeD_pt(twoD_pt(lat_N(54),long_W(40)),fl_range(fl(210),fl(210))),time(7,36,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(55),long_W(30)),fl_range(fl(230),fl(230))),time(8,20,0)),0.77)belongs_to profile_CUBE51_1.

the_Segment(profile_CUBE51_1,fourD_pt(threeD_pt(twoD_pt(lat_N(55),long_W(30)),fl_range(fl(230),fl(230))),time(8,20,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(56),long_W(20)),fl_range(fl(230),fl(230))),time(9,2,0)),0.77)belongs_to profile_CUBE51_1.

the_Segment(profile_CUBE51_1,fourD_pt(threeD_pt(twoD_pt(lat_N(56),long_W(20)),fl_range(fl(230),fl(230))),time(9,2,0)),fourD_pt(threeD_pt(twoD_pt(lat_N(56),long_W(10)),fl_range(fl(210),fl(210))),time(9,40,0)),0.77)belongs_to profile_CUBE51_1.

profiles_are_in_oceanic_conflict(Segment1,Profile1,Segment2,Profile2):-
    Segment1 belongs_to Profile1,
    Segment2 belongs_to Profile2,
    segments_are_in_oceanic_conflict(Segment1,Segment2),
    !.

segments_are_in_oceanic_conflict(Segment1,Segment2):-
    are_in_vertical_oceanic_conflict(Segment1,Segment2),
    are_in_lateral_and_longitudinal_oceanic_conflict(Segment1,Segment2),
    !.

are_subject_to_oceanic_cpr(Segment1,Segment2):-
    oceanic_separation_rules_are_applicable_to(Segment1),
    oceanic_separation_rules_are_applicable_to(Segment2),
    belong_to_profiles_for_different_aircraft(Segment1,Segment2),
    !.

oceanic_separation_rules_are_applicable_to(Segment):-
    the_Profile_containing(Segment,Profile1),
    is_wholly_or_partly_in_oca(Profile1),
    starts_at_or_after_first_recognised_pt_for_oceanic_cpr(Segment),
    ends_at_or_before_last_recognised_pt_for_oceanic_cpr(Segment),
    the_max_Flight_level_of_segment(Segment,Flight_level1),
    Flight_level1 is_at_or_above fl(55),
    ground_not__((is_wholly_outside_oca(Segment))),
    !.

starts_at_or_after_first_recognised_pt_for_oceanic_cpr(Segment):-
    the_Profile_containing(Segment,Profile1),
    is_wholly_or_partly_in_oca(Profile1),
    the_Profile_containing(Segment,Profile1),
    the_entry_Time_of(Segment,Time1),
    the_first_recognised_4D_pt_for_oceanic_cpr_of(Profile1,Four_D_pt1),
    the_Time_of(Four_D_pt1,Time2),
    Time1 is_at_or_later_than Time2,
    !.

ends_at_or_before_last_recognised_pt_for_oceanic_cpr(Segment):-
    the_Profile_containing(Segment,Profile1),
    is_wholly_or_partly_in_oca(Profile1),
    the_Profile_containing(Segment,Profile1),
    the_exit_Time_of(Segment,Time1),
    the_last_recognised_4D_pt_for_oceanic_cpr_of(Profile1,Four_D_pt1),
    the_Time_of(Four_D_pt1,Time2),
    Time1 is_at_or_earlier_than Time2,
    !.

are_in_vertical_oceanic_conflict(Segment1,Segment2):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    ( flight_level_ranges_overlap(Segment1,Segment2)
      ;
      ground_not__((flight_level_ranges_overlap(Segment1,Segment2))),
      min_flight_level_of_upper_segment_of_is_above_max_flight_level_of_lower_segment_by_less_than_required_min_vert_sep(Segment1,Segment2)
    ),
    !.

are_in_lateral_and_longitudinal_oceanic_conflict(Segment1,Segment2):-
    time_periods_overlap(Segment1,Segment2),
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    ground_not__((are_deemed_to_be_laterally_separated(Segment1,Segment2))),
    the_overlap_window_start_Time_for(Segment1,Segment2,Time1),
    the_overlap_window_end_Time_for(Segment1,Segment2,Time2),
    box_conflict_exists_between_linear_tracks_of_at_some_time_at_or_between(Segment1,Segment2,Time1,Time2),
    !.

are_in_lateral_and_longitudinal_oceanic_conflict(Segment1,Segment2):-
    ground_not__((time_periods_overlap(Segment1,Segment2))),
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    ground_not__((are_deemed_to_be_laterally_separated(Segment1,Segment2))),
    the_time_gap_Val_in_mins_between(Segment1,Segment2,Val1),
    the_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,Val2),
    Val1<Val2,
    the_later_Segment_of(Segment1,Segment2,Segment3),
    the_entry_Linear_track_pt_of(Segment3,Linear_track_pt1),
    the_earlier_Segment_of(Segment1,Segment2,Segment4),
    the_exit_Linear_track_pt_of(Segment4,Linear_track_pt2),
    is_within_conflict_box_of(Linear_track_pt1,Linear_track_pt2),
    the_later_Segment_of(Segment1,Segment2,Segment3),
    the_entry_Linear_track_pt_of(Segment3,Linear_track_pt1),
    the_earlier_Segment_of(Segment1,Segment2,Segment4),
    the_exit_Linear_track_pt_of(Segment4,Linear_track_pt2),
    is_within_conflict_box_of(Linear_track_pt2,Linear_track_pt1),
    !.

box_conflict_exists_between_linear_tracks_of_at_some_time_at_or_between(Segment1,Segment2,Time1,Time2):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    is_in_overlap_time_window_for(Time1,Segment1,Segment2),
    is_in_overlap_time_window_for(Time2,Segment1,Segment2),
    Time2 is_at_or_later_than Time1,
    ( box_conflict_exists_between_linear_tracks_of_at(Segment1,Segment2,Time1)
      ;
      the_next_integer_Time_in_mins_after(Time1,Time3),
      box_conflict_exists_between_linear_tracks_of_at_some_time_at_or_between(Segment1,Segment2,Time3,Time2)
    ),
    !.

box_conflict_exists_between_linear_tracks_of_at(Segment1,Segment2,Time):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    is_in_overlap_time_window_for(Time,Segment1,Segment2),
    position_on_linear_track_of_at_is(Segment1,Time,Linear_track_pt1),
    position_on_linear_track_of_at_is(Segment2,Time,Linear_track_pt2),
    is_within_conflict_box_of(Linear_track_pt1,Linear_track_pt2),
    is_within_conflict_box_of(Linear_track_pt2,Linear_track_pt1),
    !.

the_overlap_window_start_Time_for(Segment1,Segment2,Time1):-
    the_entry_Time_of(Segment1,Time1),
    Time1 is_during_time_period_of Segment2,
    !.

the_overlap_window_start_Time_for(Segment1,Segment2,Time1):-
    the_entry_Time_of(Segment2,Time1),
    Time1 is_during_time_period_of Segment1,
    !.

the_overlap_window_end_Time_for(Segment1,Segment2,Time1):-
    the_exit_Time_of(Segment1,Time1),
    Time1 is_during_time_period_of Segment2,
    !.

the_overlap_window_end_Time_for(Segment1,Segment2,Time1):-
    the_exit_Time_of(Segment2,Time1),
    Time1 is_during_time_period_of Segment1,
    !.

is_within_conflict_box_of(Linear_track_pt2,Linear_track_pt1):-
    the_Segment_of(Linear_track_pt1,Segment1),
    the_Segment_of(Linear_track_pt2,Segment2),
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    the_Segment_of(Linear_track_pt1,Segment1),
    regular(Segment1),
    the_Segment_of(Linear_track_pt1,Segment1),
    the_long_Val_in_arc_mins_of(Linear_track_pt2,Val1),
    is_on_linear_track_or_extended_linear_track_of_at_the_intersection_with_longitude_coord(Linear_track_pt1_,Segment1,Val1),
    the_Segment_of(Linear_track_pt1,Segment1),
    the_Segment_of(Linear_track_pt2,Segment2),
    the_nm_linear_dist_Val_between_linear_track_pts(Linear_track_pt1_,Linear_track_pt2,Val2),
    the_required_lateral_half_box_dimension_Val_for_wrt(Segment1,Segment2,Val3),
    Val2<Val3,
    the_Segment_of(Linear_track_pt1,Segment1),
    the_Segment_of(Linear_track_pt2,Segment2),
    the_nm_linear_dist_Val_between_linear_track_pts(Linear_track_pt1_,Linear_track_pt1,Val4),
    the_required_longitudinal_half_box_dimension_Val_for_wrt(Segment1,Segment2,Val5),
    Val4<Val5,
    !.

is_within_conflict_box_of(Linear_track_pt2,Linear_track_pt1):-
    the_Segment_of(Linear_track_pt1,Segment1),
    the_Segment_of(Linear_track_pt2,Segment2),
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    the_Segment_of(Linear_track_pt1,Segment1),
    irregular(Segment1),
    the_Segment_of(Linear_track_pt1,Segment1),
    the_lat_Val_in_arc_mins_of(Linear_track_pt2,Val1),
    is_on_linear_track_or_extended_linear_track_of_at_the_intersection_with_latitude_coord(Linear_track_pt1_,Segment1,Val1),
    the_Segment_of(Linear_track_pt1,Segment1),
    the_Segment_of(Linear_track_pt2,Segment2),
    the_nm_linear_dist_Val_between_linear_track_pts(Linear_track_pt1_,Linear_track_pt2,Val2),
    the_required_lateral_half_box_dimension_Val_for_wrt(Segment1,Segment2,Val3),
    Val2<Val3,
    the_Segment_of(Linear_track_pt1,Segment1),
    the_Segment_of(Linear_track_pt2,Segment2),
    the_nm_linear_dist_Val_between_linear_track_pts(Linear_track_pt1_,Linear_track_pt1,Val4),
    the_required_longitudinal_half_box_dimension_Val_for_wrt(Segment1,Segment2,Val5),
    Val4<Val5,
    !.

the_required_lateral_half_box_dimension_Val_for_wrt(Segment1,Segment2,Val2):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    steep(Segment1),
    the_min_lateral_sep_Val_in_nm_required_for(Segment1,Segment2,Val1),
    Val2 is 1.5*Val1,
    !.

the_required_lateral_half_box_dimension_Val_for_wrt(Segment1,Segment2,Val1):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    ground_not__((steep(Segment1))),
    the_min_lateral_sep_Val_in_nm_required_for(Segment1,Segment2,Val1),
    !.

the_required_longitudinal_half_box_dimension_Val_for_wrt(Segment1,Segment2,Val4):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    time_periods_overlap(Segment1,Segment2),
    the_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,Val1),
    the_nm_linear_track_length_Val_of(Segment1,Val2),
    the_time_to_fly_Val_in_mins_of(Segment1,Val3),
    Val4 is Val1*Val2/Val3,
    !.

the_required_longitudinal_half_box_dimension_Val_for_wrt(Segment1,Segment2,Val5):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    ground_not__((time_periods_overlap(Segment1,Segment2))),
    the_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,Val1),
    the_time_gap_Val_in_mins_between(Segment1,Segment2,Val2),
    the_nm_linear_track_length_Val_of(Segment1,Val3),
    the_time_to_fly_Val_in_mins_of(Segment1,Val4),
    Val5 is(Val1-Val2)*Val3/Val4,
    !.

the_time_gap_Val_in_mins_between(Segment1,Segment2,Val3):-
    finishes_at_or_earlier_than_starts(Segment1,Segment2),
    the_entry_Time_of(Segment2,Time1),
    the_total_Val_in_mins_of(Time1,Val1),
    the_exit_Time_of(Segment1,Time2),
    the_total_Val_in_mins_of(Time2,Val2),
    Val3 is Val1-Val2,
    !.

the_time_gap_Val_in_mins_between(Segment1,Segment2,Val3):-
    finishes_at_or_earlier_than_starts(Segment2,Segment1),
    the_entry_Time_of(Segment1,Time1),
    the_total_Val_in_mins_of(Time1,Val1),
    the_exit_Time_of(Segment2,Time2),
    the_total_Val_in_mins_of(Time2,Val2),
    Val3 is Val1-Val2,
    !.

the_min_vertical_sep_Val_in_feet_required_for(Flight_level1,Segment1,Flight_level2,Segment2,1000):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    Flight_level1 lies_in_flight_level_range_of Segment1,
    Flight_level2 lies_in_flight_level_range_of Segment2,
    both_are_flown_at_subsonic_speed(Segment1,Segment2),
    both_are_at_or_below(Flight_level1,Flight_level2,fl(290)),
    !.

the_min_vertical_sep_Val_in_feet_required_for(Flight_level1,Segment1,Flight_level2,Segment2,4000):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    Flight_level1 lies_in_flight_level_range_of Segment1,
    Flight_level2 lies_in_flight_level_range_of Segment2,
    one_or_both_of_are_flown_at_supersonic_speed(Segment1,Segment2),
    both_are_above(Flight_level1,Flight_level2,fl(430)),
    !.

the_min_vertical_sep_Val_in_feet_required_for(Flight_level1,Segment1,Flight_level2,Segment2,2000):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    ( both_are_flown_at_subsonic_speed(Segment1,Segment2),
      one_or_both_are_above(Flight_level1,Flight_level2,fl(290))
      ;
      one_or_both_of_are_flown_at_supersonic_speed(Segment1,Segment2),
      one_or_both_are_at_or_below(Flight_level1,Flight_level2,fl(430))
    ),
    !.

the_min_lateral_sep_Val_in_nm_required_for(Segment1,Segment2,60):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    meet_mnps(Aircraft1,Aircraft2),
    !.

the_min_lateral_sep_Val_in_nm_required_for(Segment1,Segment2,120):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    one_or_both_do_not_meet_mnps(Aircraft1,Aircraft2),
    !.

steep(Segment):-
    the_most_southerly_Lat_of(Segment,Lat1),
    Lat1 is_north_of lat_N(0),
    the_most_northerly_Lat_of(Segment,Lat2),
    Lat2 is_at_or_south_of lat_N(90),
    ( the_most_northerly_Lat_of(Segment,Lat2),
      Lat2 is_at_or_south_of lat_N(58),
      Val1 is 3/10,
      abs_magnitude_of_lat_over_long_gradient_of_exceeds(Segment,Val1)
      ;
      ( the_most_northerly_Lat_of(Segment,Lat2),
        Lat2 is_north_of lat_N(58),
        the_most_northerly_Lat_of(Segment,Lat2),
        Lat2 is_at_or_south_of lat_N(70),
        Val2 is 2/10,
        abs_magnitude_of_lat_over_long_gradient_of_exceeds(Segment,Val2)
        ;
        ( the_most_northerly_Lat_of(Segment,Lat2),
          Lat2 is_north_of lat_N(70),
          the_most_northerly_Lat_of(Segment,Lat2),
          Lat2 is_at_or_south_of lat_N(80),
          Val3 is 1/10,
          abs_magnitude_of_lat_over_long_gradient_of_exceeds(Segment,Val3)
          ;
          the_most_southerly_Lat_of(Segment,Lat1),
          Lat1 is_north_of lat_N(80)
        )
      )
    ),
    !.

are_deemed_to_be_laterally_separated(Segment1,Segment2):-
    ( are_westerly_and_deemed_to_be_laterally_separated(Segment1,Segment2)
      ;
      are_easterly_and_deemed_to_be_laterally_separated(Segment1,Segment2)
    ),
    !.

are_westerly_and_deemed_to_be_laterally_separated(Segment1,Segment2):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    is_westerly(Segment1),
    is_westerly(Segment2),
    the_entry_Long_of(Segment1,Long1),
    the_entry_Long_of(Segment2,Long2),
    same_Long(Long1,Long2),
    the_entry_Long_of(Segment1,Long1),
    the_entry_Long_of(Segment2,Long2),
    are_on_the_eastern_boundary_of_the_Shanwick_OCA(Long1,Long2),
    the_entry_Lat_of(Segment1,Lat1),
    the_entry_Lat_of(Segment2,Lat2),
    are_separated_by_at_least_one_degree(Lat1,Lat2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    one_or_both_do_not_meet_mnps(Aircraft1,Aircraft2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    the_linear_tracks_of_continuously_diverge_to_provide_at_least_120NM_separation_by_20_degrees_west(Profile1,Profile2),
    !.

are_easterly_and_deemed_to_be_laterally_separated(Segment1,Segment2):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    is_easterly(Segment1),
    is_easterly(Segment2),
    the_most_westerly_Long_of(Segment1,Long1),
    is_at_or_is_west_of_the_eastern_Shanwick_OCA_boundary(Long1),
    the_most_easterly_Long_of(Segment1,Long2),
    the_Profile_containing(Segment1,Profile1),
    is_at_the_last_recognised_pt_for_oceanic_cpr_of(Long2,Profile1),
    the_most_westerly_Long_of(Segment2,Long3),
    is_at_or_is_west_of_the_eastern_Shanwick_OCA_boundary(Long3),
    the_most_easterly_Long_of(Segment2,Long4),
    the_Profile_containing(Segment2,Profile2),
    is_at_the_last_recognised_pt_for_oceanic_cpr_of(Long4,Profile2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    are_laterally_separated_at_the_eastern_Shanwick_OCA_boundary(Profile1,Profile2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    the_linear_tracks_of_from_the_eastern_Shanwick_OCA_boundary_do_not_cross_prior_to_their_respective_last_recognised_points(Profile1,Profile2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    are_not_cleared_to_the_same_last_recognised_pt(Profile1,Profile2),
    !.

the_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,Val4):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,Val),
    the_Profile_containing(Segment1,Profile1),
    is_a_min_long_sep_value_in_mins_entered_via_the_mst_command(Profile1,Val1),
    the_Profile_containing(Segment2,Profile2),
    is_a_min_long_sep_value_in_mins_entered_via_the_mst_command(Profile2,Val2),
    the_prox_airfield_long_sep_adjustment_Val_in_mins_required_for(Segment1,Segment2,Val3),
    Val4 is max(Val,max(Val1,Val2))+Val3,
    !.

the_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,Val4):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,Val),
    ( the_Profile_containing(Segment1,Profile1),
      is_a_min_long_sep_value_in_mins_entered_via_the_mst_command(Profile1,Val1),
      the_Profile_containing(Segment2,Profile2),
      not__((is_a_min_long_sep_value_in_mins_entered_via_the_mst_command(Profile2,Val2)))
      ;
      the_Profile_containing(Segment2,Profile2),
      is_a_min_long_sep_value_in_mins_entered_via_the_mst_command(Profile2,Val1),
      the_Profile_containing(Segment1,Profile1),
      not__((is_a_min_long_sep_value_in_mins_entered_via_the_mst_command(Profile1,Val2)))
    ),
    the_prox_airfield_long_sep_adjustment_Val_in_mins_required_for(Segment1,Segment2,Val3),
    Val4 is max(Val,Val1)+Val3,
    !.

the_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,Val4):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    the_Profile_containing(Segment1,Profile1),
    not__((is_a_min_long_sep_value_in_mins_entered_via_the_mst_command(Profile1,Val1))),
    the_Profile_containing(Segment2,Profile2),
    not__((is_a_min_long_sep_value_in_mins_entered_via_the_mst_command(Profile2,Val1))),
    the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,Val2),
    the_prox_airfield_long_sep_adjustment_Val_in_mins_required_for(Segment1,Segment2,Val3),
    Val4 is Val2+Val3,
    !.

the_prox_airfield_long_sep_adjustment_Val_in_mins_required_for(Segment1,Segment2,10):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    both_aircraft_on_are_grounded_at_prox_airfields_at_time_of_current_conflict_probe(Profile1,Profile2),
    !.

the_prox_airfield_long_sep_adjustment_Val_in_mins_required_for(Segment1,Segment2,5):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    only_one_of_the_aircraft_on_is_grounded_at_a_prox_airfield_at_time_of_current_conflict_probe(Profile1,Profile2),
    !.

the_prox_airfield_long_sep_adjustment_Val_in_mins_required_for(Segment1,Segment2,0):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    neither_of_the_aircraft_on_is_grounded_at_prox_airfields_at_time_of_current_conflict_probe(Profile1,Profile2),
    !.

the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,10):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    both_are_flown_at_supersonic_speed(Segment1,Segment2),
    ( both_are_flown_at_the_same_mach_number_in_level_flight(Segment1,Segment2)
      ;
      the_Aircraft_on_segment(Segment1,Aircraft1),
      the_Type_of(Aircraft1,Type1),
      the_Aircraft_on_segment(Segment2,Aircraft2),
      the_Type_of(Aircraft2,Type2),
      Type1=Type2,
      are_cruise_climbed(Segment1,Segment2)
    ),
    ( time_of_current_conflict_probe(Time1),
      are_after_a_common_pt_from_which_profile_tracks_are_same_or_diverging_thereafter_and_at_which_both_aircraft_have_already_reported_by(Segment1,Segment2,Time1)
      ;
      are_after_a_common_pt_on_the_shanwick_boundary_from_which_profile_tracks_are_same_or_diverging_thereafter(Segment1,Segment2)
    ),
    !.

the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,15):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    both_are_flown_at_supersonic_speed(Segment1,Segment2),
    ground_not__((the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,10))),
    !.

the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,10):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    one_or_both_of_are_flown_at_subsonic_speed(Segment1,Segment2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    meet_mnps(Aircraft1,Aircraft2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    are_jets(Aircraft1,Aircraft2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    are_wholly_or_partly_in_the_mnps_airspace(Profile1,Profile2),
    reduced_separation_conditions_apply_to(Segment1,Segment2),
    the_preceding_aircraft_on_or_on_is_not_faster_by_or_more(Segment1,Segment2,2),
    !.

the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,9):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    one_or_both_of_are_flown_at_subsonic_speed(Segment1,Segment2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    meet_mnps(Aircraft1,Aircraft2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    are_jets(Aircraft1,Aircraft2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    are_wholly_or_partly_in_the_mnps_airspace(Profile1,Profile2),
    reduced_separation_conditions_apply_to(Segment1,Segment2),
    the_preceding_aircraft_on_or_on_is_faster_by(Segment1,Segment2,2),
    !.

the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,8):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    one_or_both_of_are_flown_at_subsonic_speed(Segment1,Segment2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    meet_mnps(Aircraft1,Aircraft2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    are_jets(Aircraft1,Aircraft2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    are_wholly_or_partly_in_the_mnps_airspace(Profile1,Profile2),
    reduced_separation_conditions_apply_to(Segment1,Segment2),
    the_preceding_aircraft_on_or_on_is_faster_by(Segment1,Segment2,3),
    !.

the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,7):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    one_or_both_of_are_flown_at_subsonic_speed(Segment1,Segment2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    meet_mnps(Aircraft1,Aircraft2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    are_jets(Aircraft1,Aircraft2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    are_wholly_or_partly_in_the_mnps_airspace(Profile1,Profile2),
    reduced_separation_conditions_apply_to(Segment1,Segment2),
    the_preceding_aircraft_on_or_on_is_faster_by(Segment1,Segment2,4),
    !.

the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,6):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    one_or_both_of_are_flown_at_subsonic_speed(Segment1,Segment2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    meet_mnps(Aircraft1,Aircraft2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    are_jets(Aircraft1,Aircraft2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    are_wholly_or_partly_in_the_mnps_airspace(Profile1,Profile2),
    reduced_separation_conditions_apply_to(Segment1,Segment2),
    the_preceding_aircraft_on_or_on_is_faster_by(Segment1,Segment2,5),
    !.

the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,5):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    one_or_both_of_are_flown_at_subsonic_speed(Segment1,Segment2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    meet_mnps(Aircraft1,Aircraft2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    are_jets(Aircraft1,Aircraft2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    are_wholly_or_partly_in_the_mnps_airspace(Profile1,Profile2),
    reduced_separation_conditions_apply_to(Segment1,Segment2),
    the_preceding_aircraft_on_or_on_is_faster_by_at_least(Segment1,Segment2,6),
    !.

the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,15):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    one_or_both_of_are_flown_at_subsonic_speed(Segment1,Segment2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    meet_mnps(Aircraft1,Aircraft2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    are_jets(Aircraft1,Aircraft2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    are_wholly_or_partly_in_the_mnps_airspace(Profile1,Profile2),
    ground_not__((the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,10))),
    ground_not__((the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,9))),
    ground_not__((the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,8))),
    ground_not__((the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,7))),
    ground_not__((the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,6))),
    ground_not__((the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,5))),
    !.

reduced_separation_conditions_apply_to(Segment1,Segment2):-
    one_or_both_of_are_flown_at_subsonic_speed(Segment1,Segment2),
    ( time_of_current_conflict_probe(Time1),
      are_after_a_common_pt_from_which_profile_tracks_are_same_or_diverging_thereafter_and_at_which_both_aircraft_have_already_reported_by(Segment1,Segment2,Time1)
      ;
      are_after_a_common_pt_on_the_shanwick_boundary_from_which_profile_tracks_are_same_or_diverging_thereafter(Segment1,Segment2)
    ),
    are_not_cruise_climbed(Segment1,Segment2),
    !.

the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,15):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    one_or_both_of_are_flown_at_subsonic_speed(Segment1,Segment2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    are_jets(Aircraft1,Aircraft2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    one_or_both_of_is_wholly_outside_the_mnps_airspace(Profile1,Profile2),
    ( time_of_current_conflict_probe(Time1),
      are_after_a_common_pt_from_which_profile_tracks_are_same_or_diverging_thereafter_and_at_which_both_aircraft_have_already_reported_by(Segment1,Segment2,Time1)
      ;
      are_after_a_common_pt_on_the_shanwick_boundary_from_which_profile_tracks_are_same_or_diverging_thereafter(Segment1,Segment2)
    ),
    the_preceding_aircraft_on_or_on_is_not_faster_by_or_more(Segment1,Segment2,3),
    !.

the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,10):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    one_or_both_of_are_flown_at_subsonic_speed(Segment1,Segment2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    are_jets(Aircraft1,Aircraft2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    one_or_both_of_is_wholly_outside_the_mnps_airspace(Profile1,Profile2),
    ( time_of_current_conflict_probe(Time1),
      are_after_a_common_pt_from_which_profile_tracks_are_same_or_diverging_thereafter_and_at_which_both_aircraft_have_already_reported_by(Segment1,Segment2,Time1)
      ;
      are_after_a_common_pt_on_the_shanwick_boundary_from_which_profile_tracks_are_same_or_diverging_thereafter(Segment1,Segment2)
    ),
    the_preceding_aircraft_on_or_on_is_faster_by_at_least_mach_but_not_more_than(Segment1,Segment2,3,6),
    !.

the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,5):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    one_or_both_of_are_flown_at_subsonic_speed(Segment1,Segment2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    are_jets(Aircraft1,Aircraft2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    one_or_both_of_is_wholly_outside_the_mnps_airspace(Profile1,Profile2),
    ( time_of_current_conflict_probe(Time1),
      are_after_a_common_pt_from_which_profile_tracks_are_same_or_diverging_thereafter_and_at_which_both_aircraft_have_already_reported_by(Segment1,Segment2,Time1)
      ;
      are_after_a_common_pt_on_the_shanwick_boundary_from_which_profile_tracks_are_same_or_diverging_thereafter(Segment1,Segment2)
    ),
    the_preceding_aircraft_on_or_on_is_faster_by_at_least(Segment1,Segment2,6),
    !.

the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,20):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    one_or_both_of_are_flown_at_subsonic_speed(Segment1,Segment2),
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    are_jets(Aircraft1,Aircraft2),
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    one_or_both_of_is_wholly_outside_the_mnps_airspace(Profile1,Profile2),
    ground_not__((reduced_separation_conditions_apply_to(Segment1,Segment2))),
    !.

the_basic_min_longitudinal_sep_Val_in_mins_required_for(Segment1,Segment2,30):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    ( the_Aircraft_on_segment(Segment1,Aircraft1),
      the_Type_of(Aircraft1,Type1),
      ground_not__((is_a_jet_type(Type1)))
      ;
      the_Aircraft_on_segment(Segment2,Aircraft2),
      the_Type_of(Aircraft2,Type2),
      ground_not__((is_a_jet_type(Type2)))
    ),
    !.

both_are_flown_in_level_flight(Segment1,Segment2):-
    is_flown_in_level_flight(Segment1),
    is_flown_in_level_flight(Segment2),
    !.

one_or_both_of_is_not_flown_in_level_flight(Segment1,Segment2):-
    ( ground_not__((is_flown_in_level_flight(Segment1)))
      ;
      ground_not__((is_flown_in_level_flight(Segment2)))
    ),
    !.

are_cruise_climbed(Segment1,Segment2):-
    is_cruise_climbed(Segment1),
    is_cruise_climbed(Segment2),
    !.

are_not_cruise_climbed(Segment1,Segment2):-
    ground_not__((is_cruise_climbed(Segment1))),
    ground_not__((is_cruise_climbed(Segment2))),
    !.

or_cruise_climbed(Segment1,Segment2):-
    ( is_cruise_climbed(Segment1)
      ;
      is_cruise_climbed(Segment2)
    ),
    !.

both_are_at_or_below(Flight_level1,Flight_level2,Flight_level3):-
    Flight_level1 is_at_or_below Flight_level3,
    Flight_level2 is_at_or_below Flight_level3,
    !.

both_are_above(Flight_level1,Flight_level2,Flight_level3):-
    Flight_level1 is_above Flight_level3,
    Flight_level2 is_above Flight_level3,
    !.

one_or_both_are_at_or_below(Flight_level1,Flight_level2,Flight_level3):-
    ( Flight_level1 is_at_or_below Flight_level3
      ;
      Flight_level2 is_at_or_below Flight_level3
    ),
    !.

one_or_both_are_above(Flight_level1,Flight_level2,Flight_level3):-
    ( Flight_level1 is_above Flight_level3
      ;
      Flight_level2 is_above Flight_level3
    ),
    !.

is_flown_in_level_flight(Segment):-
    the_max_entry_Flight_level_of(Segment,Flight_level1),
    the_min_entry_Flight_level_of(Segment,Flight_level2),
    same_Flight_level(Flight_level1,Flight_level2),
    the_max_exit_Flight_level_of(Segment,Flight_level3),
    the_min_exit_Flight_level_of(Segment,Flight_level4),
    same_Flight_level(Flight_level3,Flight_level4),
    the_max_entry_Flight_level_of(Segment,Flight_level1),
    the_max_exit_Flight_level_of(Segment,Flight_level3),
    same_Flight_level(Flight_level1,Flight_level3),
    !.

both_are_flown_at_supersonic_speed(Segment1,Segment2):-
    the_machno_Val_on(Segment1,Val1),
    Val1>=100,
    the_machno_Val_on(Segment2,Val2),
    Val2>=100,
    !.

one_or_both_of_are_flown_at_subsonic_speed(Segment1,Segment2):-
    ( the_machno_Val_on(Segment1,Val1),
      Val1<100
      ;
      the_machno_Val_on(Segment2,Val2),
      Val2<100
    ),
    !.

both_are_flown_at_subsonic_speed(Segment1,Segment2):-
    the_machno_Val_on(Segment1,Val1),
    Val1<100,
    the_machno_Val_on(Segment2,Val2),
    Val2<100,
    !.

one_or_both_of_are_flown_at_supersonic_speed(Segment1,Segment2):-
    ( the_machno_Val_on(Segment1,Val1),
      Val1>=100
      ;
      the_machno_Val_on(Segment2,Val2),
      Val2>=100
    ),
    !.

both_are_flown_at_the_same_mach_number(Segment1,Segment2):-
    the_machno_Val_on(Segment1,Val1),
    the_machno_Val_on(Segment2,Val2),
    Val1=Val2,
    !.

both_are_flown_at_the_same_mach_number_in_level_flight(Segment1,Segment2):-
    both_are_flown_in_level_flight(Segment1,Segment2),
    both_are_flown_at_the_same_mach_number(Segment1,Segment2),
    !.

are_jets(Aircraft1,Aircraft2):-
    the_Type_of(Aircraft1,Type1),
    is_a_jet_type(Type1),
    the_Type_of(Aircraft2,Type2),
    is_a_jet_type(Type2),
    !.

meet_mnps(Aircraft1,Aircraft2):-
    meets_mnps(Aircraft1),
    meets_mnps(Aircraft2),
    !.

one_or_both_do_not_meet_mnps(Aircraft1,Aircraft2):-
    ( ground_not__((meets_mnps(Aircraft1)))
      ;
      ground_not__((meets_mnps(Aircraft2)))
    ),
    !.

both_aircraft_on_are_grounded_at_prox_airfields_at_time_of_current_conflict_probe(Profile1,Profile2):-
    aircraft_on_is_grounded_at_a_prox_airfield_at_time_of_current_conflict_probe(Profile1),
    aircraft_on_is_grounded_at_a_prox_airfield_at_time_of_current_conflict_probe(Profile2),
    !.

only_one_of_the_aircraft_on_is_grounded_at_a_prox_airfield_at_time_of_current_conflict_probe(Profile1,Profile2):-
    ( aircraft_on_is_grounded_at_a_prox_airfield_at_time_of_current_conflict_probe(Profile1),
      ground_not__((aircraft_on_is_grounded_at_a_prox_airfield_at_time_of_current_conflict_probe(Profile2)))
      ;
      aircraft_on_is_grounded_at_a_prox_airfield_at_time_of_current_conflict_probe(Profile2),
      ground_not__((aircraft_on_is_grounded_at_a_prox_airfield_at_time_of_current_conflict_probe(Profile1)))
    ),
    !.

neither_of_the_aircraft_on_is_grounded_at_prox_airfields_at_time_of_current_conflict_probe(Profile1,Profile2):-
    ground_not__((aircraft_on_is_grounded_at_a_prox_airfield_at_time_of_current_conflict_probe(Profile1))),
    ground_not__((aircraft_on_is_grounded_at_a_prox_airfield_at_time_of_current_conflict_probe(Profile2))),
    !.

aircraft_on_is_grounded_at_a_prox_airfield_at_time_of_current_conflict_probe(Profile):-
    the_first_4D_pt_of(Profile,Four_D_pt1),
    the_2D_pt_of_4D_pt(Four_D_pt1,Two_D_pt1),
    is_a_prox_airfield_pt(Two_D_pt1),
    the_first_4D_pt_of(Profile1,Four_D_pt2),
    the_Time_of(Four_D_pt2,Time1),
    time_of_current_conflict_probe(Time2),
    Time1 is_at_or_later_than Time2,
    !.

are_wholly_or_partly_in_the_mnps_airspace(Profile1,Profile2):-
    is_wholly_or_partly_in_the_mnps_airspace(Profile1),
    is_wholly_or_partly_in_the_mnps_airspace(Profile2),
    !.

one_or_both_of_is_wholly_outside_the_mnps_airspace(Profile1,Profile2):-
    ( is_wholly_outside_the_mnps_airspace(Profile1)
      ;
      is_wholly_outside_the_mnps_airspace(Profile2)
    ),
    !.

are_wholly_within_the_mnps_airspace(Profile1,Profile2):-
    is_wholly_within_the_mnps_airspace(Profile1),
    is_wholly_within_the_mnps_airspace(Profile2),
    !.

one_or_both_of_finish_at_or_after(Segment1,Segment2,Time):-
    ( the_exit_Time_of(Segment1,Time1),
      Time1 is_at_or_later_than Time
      ;
      the_exit_Time_of(Segment2,Time2),
      Time2 is_at_or_later_than Time
    ),
    !.

both_finish_before(Segment1,Segment2,Time):-
    the_exit_Time_of(Segment1,Time1),
    Time1 is_earlier_than Time,
    the_exit_Time_of(Segment2,Time2),
    Time2 is_earlier_than Time,
    !.

is_in_overlap_time_window_for(Time,Segment1,Segment2):-
    Time is_during_time_period_of Segment1,
    Time is_during_time_period_of Segment2,
    !.

min_flight_level_of_upper_segment_of_is_above_max_flight_level_of_lower_segment_by_less_than_required_min_vert_sep(Segment1,Segment2):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    the_min_Flight_level_of_segment(Segment1,Flight_level3),
    same_Flight_level(Flight_level1,Flight_level3),
    the_max_Flight_level_of_segment(Segment2,Flight_level4),
    same_Flight_level(Flight_level2,Flight_level4),
    Flight_level1 is_above Flight_level2,
    the_Val_in_feet_of(Flight_level1,Val1),
    the_Val_in_feet_of(Flight_level2,Val2),
    Val3 is Val1-Val2,
    the_min_vertical_sep_Val_in_feet_required_for(Flight_level1,Segment1,Flight_level2,Segment2,Val4),
    Val3<Val4,
    !.

min_flight_level_of_upper_segment_of_is_above_max_flight_level_of_lower_segment_by_less_than_required_min_vert_sep(Segment1,Segment2):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    the_max_Flight_level_of_segment(Segment1,Flight_level3),
    same_Flight_level(Flight_level1,Flight_level3),
    the_min_Flight_level_of_segment(Segment2,Flight_level4),
    same_Flight_level(Flight_level2,Flight_level4),
    Flight_level2 is_above Flight_level1,
    the_Val_in_feet_of(Flight_level2,Val1),
    the_Val_in_feet_of(Flight_level1,Val2),
    Val3 is Val1-Val2,
    the_min_vertical_sep_Val_in_feet_required_for(Flight_level1,Segment1,Flight_level2,Segment2,Val4),
    Val3<Val4,
    !.

abs_magnitude_of_lat_over_long_gradient_of_exceeds(Segment,Val):-
    ( the_entry_Long_of(Segment,Long1),
      the_exit_Long_of(Segment,Long2),
      same_Long(Long1,Long2)
      ;
      the_entry_Long_of(Segment,Long1),
      the_exit_Long_of(Segment,Long2),
      ground_not__((same_Long(Long1,Long2))),
      the_lat_over_long_gradient_Val_of(Segment,Val1),
      Val2 is abs(Val1),
      Val2>Val
    ),
    !.

position_on_linear_track_of_at_is(Segment,Time,the_Linear_track_pt(Segment,Val)):-
    the_total_Val_in_mins_of(Time,Val1),
    Val=Val1,
    !.

is_on_linear_track_or_extended_linear_track_of_at_the_intersection_with_longitude_coord(the_Linear_track_pt(Segment,Val1),Segment,Val):-
    the_entry_Long_of(Segment,Long1),
    the_exit_Long_of(Segment,Long2),
    ground_not__((same_Long(Long1,Long2))),
    the_entry_time_Val_in_mins_of(Segment,Val2),
    the_time_to_fly_Val_in_mins_of(Segment,Val3),
    the_entry_long_Val_in_arc_mins_of(Segment,Val4),
    the_exit_long_Val_in_arc_mins_of(Segment,Val5),
    Val6 is Val2+Val3*((Val-Val4)/(Val5-Val4)),
    Val1=Val6,
    !.

the_lat_Val_on_linear_track_of_at(Segment,Long,Val4):-
    the_entry_Long_of(Segment,Long1),
    the_exit_Long_of(Segment,Long2),
    ground_not__((same_Long(Long1,Long2))),
    the_total_Val_in_arc_mins_of_long(Long,Val1),
    the_offset_const_Val_for_linear_track_of(Segment,Val2),
    the_lat_over_long_gradient_Val_of(Segment,Val3),
    Val4 is(Val1-Val2)/Val3,
    !.

is_on_linear_track_or_extended_linear_track_of_at_the_intersection_with_latitude_coord(the_Linear_track_pt(Segment,Val1),Segment,Val):-
    the_entry_Lat_of(Segment,Lat1),
    the_exit_Lat_of(Segment,Lat2),
    ground_not__((same_Lat(Lat1,Lat2))),
    the_entry_time_Val_in_mins_of(Segment,Val2),
    the_time_to_fly_Val_in_mins_of(Segment,Val3),
    the_entry_lat_Val_in_arc_mins_of(Segment,Val4),
    the_exit_lat_Val_in_arc_mins_of(Segment,Val5),
    Val6 is Val2+Val3*((Val-Val4)/(Val5-Val4)),
    Val1=Val6,
    !.

are_separated_by_at_least_one_degree(Lat1,Lat2):-
    the_total_Val_in_arc_mins_of_lat(Lat1,Val1),
    the_total_Val_in_arc_mins_of_lat(Lat2,Val2),
    Val3 is abs(Val1-Val2),
    Val3>=60,
    !.

the_linear_tracks_of_continuously_diverge_to_provide_at_least_120NM_separation_by_20_degrees_west(Profile1,Profile2):-
    the_linear_tracks_of_diverge_continuously(Profile1,Profile2),
    the_linear_tracks_of_are_separated_by_at_least_120NM_at_20_degrees_west(Profile1,Profile2),
    !.

the_linear_tracks_of_are_separated_by_at_least_120NM_at_20_degrees_west(Profile1,Profile2):-
    Segment1 belongs_to Profile1,
    Segment2 belongs_to Profile2,
    is_on_the_linear_track_of(Two_D_pt1,Segment1),
    is_on_the_linear_track_of(Two_D_pt2,Segment2),
    the_Long_of_2D_pt(Two_D_pt1,Long1),
    same_Long(Long1,long_W(20)),
    the_Long_of_2D_pt(Two_D_pt2,Long2),
    same_Long(Long2,long_W(20)),
    the_lat_Val_in_arc_mins_of(Two_D_pt1,Val1),
    the_lat_Val_in_arc_mins_of(Two_D_pt2,Val2),
    Val3 is abs(Val1-Val2),
    Val3>=120,
    !.

is_a_2D_pt_west_of_the_eastern_Shanwick_OCA_boundary(Two_D_pt):-
    ( the_Lat_of_2D_pt(Two_D_pt,Lat1),
      Lat1 is_at_or_north_of lat_N(54),
      the_Long_of_2D_pt(Two_D_pt,Long1),
      Long1 is_at_or_west_of long_W(10)
      ;
      ( the_Lat_of_2D_pt(Two_D_pt,Lat1),
        Lat1 is_at_or_south_of lat_N(49),
        the_Long_of_2D_pt(Two_D_pt,Long1),
        Long1 is_at_or_west_of long_W(8)
        ;
        the_Lat_of_2D_pt(Two_D_pt,Lat1),
        Lat1 is_at_or_south_of lat_N(54),
        the_Lat_of_2D_pt(Two_D_pt,Lat1),
        Lat1 is_at_or_north_of lat_N(49),
        the_Long_of_2D_pt(Two_D_pt,Long1),
        Long1 is_at_or_west_of long_W(15)
      )
    ),
    !.

is_at_the_last_recognised_pt_for_oceanic_cpr_of(Long,Profile):-
    the_total_Val_in_arc_mins_of_long(Long,Val1),
    the_last_recognised_4D_pt_for_oceanic_cpr_of(Profile,Four_D_pt1),
    the_Long_of_4D_pt(Four_D_pt1,Long1),
    the_total_Val_in_arc_mins_of_long(Long1,Val2),
    Val1=Val2,
    !.

are_laterally_separated_at_the_eastern_Shanwick_OCA_boundary(Profile1,Profile2):-
    Segment1 belongs_to Profile1,
    Segment2 belongs_to Profile2,
    starts_at_or_crosses_the_eastern_Shanwick_OCA_boundary(Segment1),
    starts_at_or_crosses_the_eastern_Shanwick_OCA_boundary(Segment2),
    are_laterally_separated(Segment1,Segment2),
    !.

are_not_cleared_to_the_same_last_recognised_pt(Profile1,Profile2):-
    the_last_recognised_4D_pt_for_oceanic_cpr_of(Profile1,Four_D_pt1),
    the_last_recognised_4D_pt_for_oceanic_cpr_of(Profile2,Four_D_pt2),
    ground_not__((same_4D_pt(Four_D_pt1,Four_D_pt2))),
    !.

the_linear_tracks_of_diverge_continuously(Profile1,Profile2):-
    not__((Segment1 belongs_to Profile1,
        Segment2 belongs_to Profile2,
        ground_not__((the_linear_tracks_of_diverge(Segment1,Segment2))))),
    !.

the_linear_tracks_of_diverge(Segment1,Segment2):-
    ground_not__((the_linear_tracks_of_cross(Segment1,Segment2))),
    the_exit_2D_pt_of(Segment1,Two_D_pt1),
    the_exit_2D_pt_of(Segment2,Two_D_pt2),
    the_nm_linear_dist_Val_between_2D_pts(Two_D_pt1,Two_D_pt2,Val1),
    the_entry_2D_pt_of(Segment1,Two_D_pt3),
    the_entry_2D_pt_of(Segment2,Two_D_pt4),
    the_nm_linear_dist_Val_between_2D_pts(Two_D_pt3,Two_D_pt4,Val2),
    Val1>Val2,
    ( the_exit_2D_pt_of(Segment1,Two_D_pt1),
      the_entry_2D_pt_of(Segment1,Two_D_pt3),
      the_entry_2D_pt_of(Segment2,Two_D_pt4),
      the_nm_linear_dist_Val_between_2D_pts(Two_D_pt3,Two_D_pt4,Val2),
      the_nm_linear_dist_Val_between_2D_pts(Two_D_pt1,Two_D_pt4,Val3),
      Val2<Val3
      ;
      the_exit_2D_pt_of(Segment2,Two_D_pt2),
      the_entry_2D_pt_of(Segment1,Two_D_pt3),
      the_entry_2D_pt_of(Segment2,Two_D_pt4),
      the_nm_linear_dist_Val_between_2D_pts(Two_D_pt3,Two_D_pt4,Val2),
      the_nm_linear_dist_Val_between_2D_pts(Two_D_pt3,Two_D_pt2,Val4),
      Val2<Val4
    ),
    !.

is_on_the_linear_track_of(Two_D_pt,Segment):-
    the_nm_linear_track_length_Val_of(Segment,Val1),
    the_entry_2D_pt_of(Segment,Two_D_pt1),
    the_nm_linear_dist_Val_between_2D_pts(Two_D_pt1,Two_D_pt,Val2),
    the_exit_2D_pt_of(Segment,Two_D_pt2),
    the_nm_linear_dist_Val_between_2D_pts(Two_D_pt2,Two_D_pt,Val3),
    Val4 is Val2+Val3,
    Val1=Val4,
    !.

are_after_a_common_pt_from_which_profile_tracks_are_same_or_diverging_thereafter_and_at_which_both_aircraft_have_already_reported_by(Segment1,Segment2,Time):-
    ( are_after_a_common_pt_from_which_profile_tracks_are_same_thereafter(Segment1,Segment2)
      ;
      are_after_a_common_pt_from_which_profile_tracks_are_diverging_thereafter(Segment1,Segment2)
    ),
    the_Profile_containing(Segment1,Profile1),
    Segment3 belongs_to Profile1,
    the_Profile_containing(Segment2,Profile2),
    Segment4 belongs_to Profile2,
    the_entry_2D_pt_of(Segment3,Two_D_pt1),
    the_entry_2D_pt_of(Segment4,Two_D_pt2),
    same_2D_pt(Two_D_pt1,Two_D_pt2),
    the_entry_Time_of(Segment3,Time1),
    Time is_at_or_later_than Time1,
    the_entry_Time_of(Segment4,Time2),
    Time is_at_or_later_than Time2,
    ( are_after_a_common_pt_from_which_profile_tracks_are_same_thereafter(Segment3,Segment4)
      ;
      are_after_a_common_pt_from_which_profile_tracks_are_diverging_thereafter(Segment3,Segment4)
    ),
    !.

the_preceding_aircraft_on_or_on_is_not_faster_by_or_more(Segment1,Segment2,Val):-
    ( are_after_a_common_pt_from_which_profile_tracks_are_same_thereafter(Segment1,Segment2)
      ;
      are_after_a_common_pt_from_which_profile_tracks_are_diverging_thereafter(Segment1,Segment2)
    ),
    ( the_aircraft_on_segment1_precedes_the_aircraft_on_segment2(Segment1,Segment2),
      the_machno_Val_on(Segment1,Val1),
      the_machno_Val_on(Segment2,Val2),
      Val3 is Val1-Val2,
      Val3<Val
      ;
      the_aircraft_on_segment1_precedes_the_aircraft_on_segment2(Segment2,Segment1),
      the_machno_Val_on(Segment1,Val1),
      the_machno_Val_on(Segment2,Val2),
      Val4 is Val2-Val1,
      Val4<Val
    ),
    !.

the_aircraft_on_segment1_precedes_the_aircraft_on_segment2(Segment1,Segment2):-
    are_after_a_common_pt_from_which_profile_tracks_are_same_thereafter(Segment1,Segment2),
    the_Profile_containing(Segment2,Profile1),
    Segment3 belongs_to Profile1,
    the_entry_2D_pt_of(Segment3,Two_D_pt1),
    the_entry_2D_pt_of(Segment1,Two_D_pt2),
    same_2D_pt(Two_D_pt1,Two_D_pt2),
    the_exit_2D_pt_of(Segment3,Two_D_pt3),
    the_exit_2D_pt_of(Segment1,Two_D_pt4),
    same_2D_pt(Two_D_pt3,Two_D_pt4),
    the_entry_Time_of(Segment3,Time1),
    the_entry_Time_of(Segment1,Time2),
    Time1 is_later_than Time2,
    !.

the_aircraft_on_segment1_precedes_the_aircraft_on_segment2(Segment1,Segment2):-
    are_after_a_common_pt_from_which_profile_tracks_are_diverging_thereafter(Segment1,Segment2),
    the_Profile_containing(Segment1,Profile1),
    Segment3 belongs_to Profile1,
    the_Profile_containing(Segment2,Profile2),
    Segment4 belongs_to Profile2,
    the_entry_2D_pt_of(Segment3,Two_D_pt1),
    the_entry_2D_pt_of(Segment4,Two_D_pt2),
    same_2D_pt(Two_D_pt1,Two_D_pt2),
    the_exit_2D_pt_of(Segment3,Two_D_pt3),
    the_exit_2D_pt_of(Segment4,Two_D_pt4),
    ground_not__((same_2D_pt(Two_D_pt3,Two_D_pt4))),
    are_after_a_common_pt_from_which_profile_tracks_are_diverging_thereafter(Segment3,Segment4),
    the_entry_Time_of(Segment3,Time1),
    the_entry_Time_of(Segment4,Time2),
    Time1 is_earlier_than Time2,
    !.

the_preceding_aircraft_on_or_on_is_faster_by(Segment1,Segment2,Val):-
    ( are_after_a_common_pt_from_which_profile_tracks_are_same_thereafter(Segment1,Segment2)
      ;
      are_after_a_common_pt_from_which_profile_tracks_are_diverging_thereafter(Segment1,Segment2)
    ),
    ( the_aircraft_on_segment1_precedes_the_aircraft_on_segment2(Segment1,Segment2),
      the_machno_Val_on(Segment1,Val1),
      the_machno_Val_on(Segment2,Val2),
      Val3 is Val1-Val2,
      Val3=Val
      ;
      the_aircraft_on_segment1_precedes_the_aircraft_on_segment2(Segment2,Segment1),
      the_machno_Val_on(Segment1,Val1),
      the_machno_Val_on(Segment2,Val2),
      Val4 is Val2-Val1,
      Val4=Val
    ),
    !.

the_preceding_aircraft_on_or_on_is_faster_by_at_least(Segment1,Segment2,Val):-
    ( are_after_a_common_pt_from_which_profile_tracks_are_same_thereafter(Segment1,Segment2)
      ;
      are_after_a_common_pt_from_which_profile_tracks_are_diverging_thereafter(Segment1,Segment2)
    ),
    ( the_aircraft_on_segment1_precedes_the_aircraft_on_segment2(Segment1,Segment2),
      the_machno_Val_on(Segment1,Val1),
      the_machno_Val_on(Segment2,Val2),
      Val3 is Val1-Val2,
      Val3>=Val
      ;
      the_aircraft_on_segment1_precedes_the_aircraft_on_segment2(Segment2,Segment1),
      the_machno_Val_on(Segment1,Val1),
      the_machno_Val_on(Segment2,Val2),
      Val4 is Val2-Val1,
      Val4>=Val
    ),
    !.

the_preceding_aircraft_on_or_on_is_faster_by_at_least_mach_but_not_more_than(Segment1,Segment2,Val1,Val2):-
    ( are_after_a_common_pt_from_which_profile_tracks_are_same_thereafter(Segment1,Segment2)
      ;
      are_after_a_common_pt_from_which_profile_tracks_are_diverging_thereafter(Segment1,Segment2)
    ),
    ( the_aircraft_on_segment1_precedes_the_aircraft_on_segment2(Segment1,Segment2),
      the_machno_Val_on(Segment1,Val3),
      the_machno_Val_on(Segment2,Val4),
      Val5 is Val3-Val4,
      Val1=<Val5,
      the_machno_Val_on(Segment1,Val3),
      the_machno_Val_on(Segment2,Val4),
      Val5 is Val3-Val4,
      Val5<Val2
      ;
      the_aircraft_on_segment1_precedes_the_aircraft_on_segment2(Segment2,Segment1),
      the_machno_Val_on(Segment1,Val3),
      the_machno_Val_on(Segment2,Val4),
      Val6 is Val4-Val3,
      Val1=<Val6,
      the_machno_Val_on(Segment1,Val3),
      the_machno_Val_on(Segment2,Val4),
      Val6 is Val4-Val3,
      Val6<Val2
    ),
    !.

are_after_a_common_pt_from_which_profile_tracks_are_same_thereafter(Segment1,Segment2):-
    the_Profile_containing(Segment1,Profile1),
    Segment3 belongs_to Profile1,
    the_Profile_containing(Segment2,Profile2),
    Segment4 belongs_to Profile2,
    the_entry_Time_of(Segment1,Time1),
    the_entry_Time_of(Segment3,Time2),
    Time1 is_at_or_later_than Time2,
    the_entry_Time_of(Segment2,Time3),
    the_entry_Time_of(Segment4,Time4),
    Time3 is_at_or_later_than Time4,
    not__((the_Profile_containing(Segment3,Profile3),
        Segment5 belongs_to Profile3,
        the_entry_Time_of(Segment3,Time2),
        the_entry_Time_of(Segment5,Time5),
        Time5 is_at_or_later_than Time2,
        not__((the_Profile_containing(Segment4,Profile4),
            Segment6 belongs_to Profile4,
            the_entry_Time_of(Segment4,Time4),
            the_entry_Time_of(Segment6,Time6),
            Time6 is_at_or_later_than Time4,
            the_linear_tracks_of_are_the_same(Segment5,Segment6))))),
    not__((the_Profile_containing(Segment4,Profile4),
        Segment8 belongs_to Profile4,
        the_entry_Time_of(Segment4,Time4),
        the_entry_Time_of(Segment8,Time7),
        Time7 is_at_or_later_than Time4,
        not__((the_Profile_containing(Segment3,Profile3),
            Segment7 belongs_to Profile3,
            the_entry_Time_of(Segment3,Time2),
            the_entry_Time_of(Segment7,Time8),
            Time8 is_at_or_later_than Time2,
            the_linear_tracks_of_are_the_same(Segment8,Segment7))))),
    !.

are_after_a_common_pt_from_which_profile_tracks_are_diverging_thereafter(Segment1,Segment2):-
    the_Profile_containing(Segment1,Profile1),
    Segment3 belongs_to Profile1,
    the_Profile_containing(Segment2,Profile2),
    Segment4 belongs_to Profile2,
    the_entry_Time_of(Segment1,Time1),
    the_entry_Time_of(Segment3,Time2),
    Time1 is_at_or_later_than Time2,
    the_entry_Time_of(Segment2,Time3),
    the_entry_Time_of(Segment4,Time4),
    Time3 is_at_or_later_than Time4,
    not__((the_Profile_containing(Segment3,Profile3),
        Segment5 belongs_to Profile3,
        the_entry_Time_of(Segment3,Time2),
        the_entry_Time_of(Segment5,Time5),
        Time5 is_at_or_later_than Time2,
        not__((the_Profile_containing(Segment4,Profile4),
            Segment6 belongs_to Profile4,
            the_entry_Time_of(Segment4,Time4),
            the_entry_Time_of(Segment6,Time6),
            Time6 is_at_or_later_than Time4,
            the_linear_tracks_of_diverge(Segment5,Segment6))))),
    not__((the_Profile_containing(Segment4,Profile4),
        Segment8 belongs_to Profile4,
        the_entry_Time_of(Segment4,Time4),
        the_entry_Time_of(Segment8,Time7),
        Time7 is_at_or_later_than Time4,
        not__((the_Profile_containing(Segment3,Profile3),
            Segment7 belongs_to Profile3,
            the_entry_Time_of(Segment3,Time2),
            the_entry_Time_of(Segment7,Time8),
            Time8 is_at_or_later_than Time2,
            the_linear_tracks_of_diverge(Segment8,Segment7))))),
    !.

the_linear_tracks_of_are_the_same(Segment1,Segment2):-
    the_entry_2D_pt_of(Segment1,Two_D_pt1),
    the_entry_2D_pt_of(Segment2,Two_D_pt2),
    same_2D_pt(Two_D_pt1,Two_D_pt2),
    the_exit_2D_pt_of(Segment1,Two_D_pt3),
    the_exit_2D_pt_of(Segment2,Two_D_pt4),
    same_2D_pt(Two_D_pt3,Two_D_pt4),
    !.

are_after_a_common_pt_on_the_shanwick_boundary_from_which_profile_tracks_are_same_or_diverging_thereafter(Segment1,Segment2):-
    ( are_after_a_common_pt_on_the_shanwick_boundary_from_which_profile_tracks_are_same_thereafter(Segment1,Segment2)
      ;
      are_after_a_common_pt_on_the_shanwick_boundary_from_which_profile_tracks_are_diverging_thereafter(Segment1,Segment2)
    ),
    !.

are_after_a_common_pt_on_the_shanwick_boundary_from_which_profile_tracks_are_same_thereafter(Segment1,Segment2):-
    the_Profile_containing(Segment1,Profile1),
    Segment3 belongs_to Profile1,
    the_Profile_containing(Segment2,Profile2),
    Segment4 belongs_to Profile2,
    the_entry_Time_of(Segment1,Time1),
    the_entry_Time_of(Segment3,Time2),
    Time1 is_at_or_later_than Time2,
    the_entry_Time_of(Segment2,Time3),
    the_entry_Time_of(Segment4,Time4),
    Time3 is_at_or_later_than Time4,
    the_entry_2D_pt_of(Segment3,Two_D_pt1),
    the_entry_2D_pt_of(Segment4,Two_D_pt2),
    same_2D_pt(Two_D_pt1,Two_D_pt2),
    the_entry_2D_pt_of(Segment3,Two_D_pt1),
    is_on_the_Shanwick_OCA_boundary(Two_D_pt1),
    not__((the_Profile_containing(Segment3,Profile3),
        Segment5 belongs_to Profile3,
        the_entry_Time_of(Segment3,Time2),
        the_entry_Time_of(Segment5,Time5),
        Time5 is_at_or_later_than Time2,
        not__((the_Profile_containing(Segment4,Profile4),
            Segment6 belongs_to Profile4,
            the_entry_Time_of(Segment4,Time4),
            the_entry_Time_of(Segment6,Time6),
            Time6 is_at_or_later_than Time4,
            the_linear_tracks_of_are_the_same(Segment5,Segment6))))),
    not__((the_Profile_containing(Segment4,Profile4),
        Segment8 belongs_to Profile4,
        the_entry_Time_of(Segment4,Time4),
        the_entry_Time_of(Segment8,Time7),
        Time7 is_at_or_later_than Time4,
        not__((the_Profile_containing(Segment3,Profile3),
            Segment7 belongs_to Profile3,
            the_entry_Time_of(Segment3,Time2),
            the_entry_Time_of(Segment7,Time8),
            Time8 is_at_or_later_than Time2,
            the_linear_tracks_of_are_the_same(Segment8,Segment7))))),
    !.

are_after_a_common_pt_on_the_shanwick_boundary_from_which_profile_tracks_are_diverging_thereafter(Segment1,Segment2):-
    the_Profile_containing(Segment1,Profile1),
    Segment3 belongs_to Profile1,
    the_Profile_containing(Segment2,Profile2),
    Segment4 belongs_to Profile2,
    the_entry_Time_of(Segment1,Time1),
    the_entry_Time_of(Segment3,Time2),
    Time1 is_at_or_later_than Time2,
    the_entry_Time_of(Segment2,Time3),
    the_entry_Time_of(Segment4,Time4),
    Time3 is_at_or_later_than Time4,
    the_entry_2D_pt_of(Segment3,Two_D_pt1),
    the_entry_2D_pt_of(Segment4,Two_D_pt2),
    same_2D_pt(Two_D_pt1,Two_D_pt2),
    the_entry_2D_pt_of(Segment3,Two_D_pt1),
    is_on_the_Shanwick_OCA_boundary(Two_D_pt1),
    not__((the_Profile_containing(Segment3,Profile3),
        Segment5 belongs_to Profile3,
        the_entry_Time_of(Segment3,Time2),
        the_entry_Time_of(Segment5,Time5),
        Time5 is_at_or_later_than Time2,
        not__((the_Profile_containing(Segment4,Profile4),
            Segment6 belongs_to Profile4,
            the_entry_Time_of(Segment4,Time4),
            the_entry_Time_of(Segment6,Time6),
            Time6 is_at_or_later_than Time4,
            the_linear_tracks_of_diverge(Segment5,Segment6))))),
    not__((the_Profile_containing(Segment4,Profile4),
        Segment8 belongs_to Profile4,
        the_entry_Time_of(Segment4,Time4),
        the_entry_Time_of(Segment8,Time7),
        Time7 is_at_or_later_than Time4,
        not__((the_Profile_containing(Segment3,Profile3),
            Segment7 belongs_to Profile3,
            the_entry_Time_of(Segment3,Time2),
            the_entry_Time_of(Segment7,Time8),
            Time8 is_at_or_later_than Time2,
            the_linear_tracks_of_diverge(Segment8,Segment7))))),
    !.

is_on_the_Shanwick_OCA_boundary(Two_D_pt):-
    ( is_on_the_eastern_boundary_of_the_Shanwick_OCA(Two_D_pt)
      ;
      ( is_on_the_western_boundary_of_the_Shanwick_OCA(Two_D_pt)
        ;
        ( is_on_the_northern_boundary_of_the_Shanwick_OCA(Two_D_pt)
          ;
          is_on_the_southern_boundary_of_the_Shanwick_OCA(Two_D_pt)
        )
      )
    ),
    !.

is_on_the_eastern_boundary_of_the_Shanwick_OCA(Two_D_pt):-
    ( the_Long_of_2D_pt(Two_D_pt,Long1),
      same_Long(Long1,long_W(10)),
      the_Lat_of_2D_pt(Two_D_pt,Lat1),
      lat_N(61)is_at_or_north_of Lat1,
      the_Lat_of_2D_pt(Two_D_pt,Lat1),
      Lat1 is_at_or_north_of lat_N(54,34)
      ;
      ( the_Long_of_2D_pt(Two_D_pt,Long1),
        same_Long(Long1,long_W(8)),
        the_Lat_of_2D_pt(Two_D_pt,Lat1),
        lat_N(48,30)is_at_or_north_of Lat1,
        the_Lat_of_2D_pt(Two_D_pt,Lat1),
        Lat1 is_at_or_north_of lat_N(45)
        ;
        ( the_Long_of_2D_pt(Two_D_pt,Long1),
          same_Long(Long1,long_W(15)),
          the_Lat_of_2D_pt(Two_D_pt,Lat1),
          lat_N(54)is_at_or_north_of Lat1,
          the_Lat_of_2D_pt(Two_D_pt,Lat1),
          Lat1 is_at_or_north_of lat_N(49)
          ;
          ( the_Long_of_2D_pt(Two_D_pt,Long1),
            long_W(15)is_at_or_west_of Long1,
            the_Long_of_2D_pt(Two_D_pt,Long1),
            Long1 is_at_or_west_of long_W(10),
            the_Lat_of_2D_pt(Two_D_pt,Lat1),
            lat_N(54,34)is_at_or_north_of Lat1,
            the_Lat_of_2D_pt(Two_D_pt,Lat1),
            Lat1 is_at_or_north_of lat_N(54)
            ;
            the_Long_of_2D_pt(Two_D_pt,Long1),
            long_W(15)is_at_or_west_of Long1,
            the_Long_of_2D_pt(Two_D_pt,Long1),
            Long1 is_at_or_west_of long_W(8),
            the_Lat_of_2D_pt(Two_D_pt,Lat1),
            lat_N(49)is_at_or_north_of Lat1,
            the_Lat_of_2D_pt(Two_D_pt,Lat1),
            Lat1 is_at_or_north_of lat_N(48,30)
          )
        )
      )
    ),
    !.

is_on_the_western_boundary_of_the_Shanwick_OCA(Two_D_pt):-
    the_Long_of_2D_pt(Two_D_pt,Long1),
    same_Long(Long1,long_W(30)),
    the_Lat_of_2D_pt(Two_D_pt,Lat1),
    lat_N(61)is_at_or_north_of Lat1,
    the_Lat_of_2D_pt(Two_D_pt,Lat1),
    Lat1 is_at_or_north_of lat_N(45),
    !.

is_on_the_northern_boundary_of_the_Shanwick_OCA(Two_D_pt):-
    the_Lat_of_2D_pt(Two_D_pt,Lat1),
    same_Lat(Lat1,lat_N(61)),
    the_Long_of_2D_pt(Two_D_pt,Long1),
    Long1 is_at_or_west_of long_W(10),
    the_Long_of_2D_pt(Two_D_pt,Long1),
    long_W(30)is_at_or_west_of Long1,
    !.

is_on_the_southern_boundary_of_the_Shanwick_OCA(Two_D_pt):-
    the_Lat_of_2D_pt(Two_D_pt,Lat1),
    same_Lat(Lat1,lat_N(45)),
    the_Long_of_2D_pt(Two_D_pt,Long1),
    Long1 is_at_or_west_of long_W(8),
    the_Long_of_2D_pt(Two_D_pt,Long1),
    long_W(30)is_at_or_west_of Long1,
    !.

are_laterally_separated(Segment1,Segment2):-
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    the_entry_Linear_track_pt_of(Segment1,Linear_track_pt1),
    the_entry_Linear_track_pt_of(Segment2,Linear_track_pt2),
    is_not_in_lateral_conflict_with(Linear_track_pt1,Linear_track_pt2),
    the_entry_Linear_track_pt_of(Segment1,Linear_track_pt1),
    the_exit_Linear_track_pt_of(Segment2,Linear_track_pt3),
    is_not_in_lateral_conflict_with(Linear_track_pt1,Linear_track_pt3),
    the_entry_Linear_track_pt_of(Segment2,Linear_track_pt2),
    the_exit_Linear_track_pt_of(Segment1,Linear_track_pt4),
    is_not_in_lateral_conflict_with(Linear_track_pt4,Linear_track_pt2),
    the_exit_Linear_track_pt_of(Segment2,Linear_track_pt3),
    the_exit_Linear_track_pt_of(Segment1,Linear_track_pt4),
    is_not_in_lateral_conflict_with(Linear_track_pt4,Linear_track_pt3),
    !.

is_not_in_lateral_conflict_with(Linear_track_pt2,Linear_track_pt1):-
    the_Segment_of(Linear_track_pt1,Segment3),
    same_Segment(Segment1,Segment3),
    the_Segment_of(Linear_track_pt2,Segment4),
    same_Segment(Segment2,Segment4),
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    regular(Segment1),
    not__((the_long_Val_in_arc_mins_of(Linear_track_pt2,Val1),
        is_on_linear_track_or_extended_linear_track_of_at_the_intersection_with_longitude_coord(Linear_track_pt1_,Segment1,Val1),
        the_nm_linear_dist_Val_between_linear_track_pts(Linear_track_pt1_,Linear_track_pt2,Val2),
        the_required_lateral_half_box_dimension_Val_for_wrt(Segment1,Segment2,Val3),
        Val2<Val3)),
    !.

is_not_in_lateral_conflict_with(Linear_track_pt2,Linear_track_pt1):-
    the_Segment_of(Linear_track_pt1,Segment3),
    same_Segment(Segment1,Segment3),
    the_Segment_of(Linear_track_pt2,Segment4),
    same_Segment(Segment2,Segment4),
    are_subject_to_oceanic_cpr(Segment1,Segment2),
    irregular(Segment1),
    not__((the_lat_Val_in_arc_mins_of(Linear_track_pt2,Val1),
        is_on_linear_track_or_extended_linear_track_of_at_the_intersection_with_latitude_coord(Linear_track_pt1_,Segment1,Val1),
        the_nm_linear_dist_Val_between_linear_track_pts(Linear_track_pt1_,Linear_track_pt2,Val2),
        the_required_lateral_half_box_dimension_Val_for_wrt(Segment1,Segment2,Val3),
        Val2<Val3)),
    !.

the_long_arc_mins_dist_Val_between(Val1,Val2,Val5):-
    0=<Val1,
    Val1<21600,
    0=<Val2,
    Val2<21600,
    the_long_arc_mins_eastward_Val_from(Val1,Val2,Val3),
    the_long_arc_mins_westward_Val_from(Val1,Val2,Val4),
    Val5 is min(Val3,Val4),
    !.

the_long_arc_mins_eastward_Val_from(Val1,Val2,Val3):-
    0=<Val1,
    Val1<21600,
    0=<Val2,
    Val2<21600,
    Val3 is max(Val2-Val1,round(Val2+(21600-Val1))mod 21600),
    !.

the_long_arc_mins_westward_Val_from(Val1,Val2,Val3):-
    0=<Val1,
    Val1<21600,
    0=<Val2,
    Val2<21600,
    the_long_arc_mins_eastward_Val_from(Val2,Val1,Val3),
    !.

same_Lat(Lat1,Lat2):-
    the_total_Val_in_arc_mins_of_lat(Lat1,Val1),
    the_total_Val_in_arc_mins_of_lat(Lat2,Val2),
    Val1=Val2,
    !.

the_total_Val_in_arc_mins_of_lat(lat_N(Int_0_to_90),Val1):-
    Val1 is Int_0_to_90*60,
    !.

the_total_Val_in_arc_mins_of_lat(lat_S(Int_0_to_90),Val1):-
    Int_0_to_90*60-Val1,
    !.

the_total_Val_in_arc_mins_of_lat(lat_N(Int_0_to_89,Int_0_to_59),Val1):-
    Val1 is Int_0_to_89*60+Int_0_to_59,
    !.

the_total_Val_in_arc_mins_of_lat(lat_S(Int_0_to_89,Int_0_to_59),Val1):-
    Val1 is (Int_0_to_89 * -60) - Int_0_to_59,
    !.

Lat1 is_north_of Lat2:-
    the_total_Val_in_arc_mins_of_lat(Lat1,Val1),
    the_total_Val_in_arc_mins_of_lat(Lat2,Val2),
    Val1>Val2,
    !.

Lat1 is_at_or_north_of Lat2:-
    the_total_Val_in_arc_mins_of_lat(Lat1,Val1),
    the_total_Val_in_arc_mins_of_lat(Lat2,Val2),
    Val1>=Val2,
    !.

Lat1 is_south_of Lat2:-
    the_total_Val_in_arc_mins_of_lat(Lat1,Val1),
    the_total_Val_in_arc_mins_of_lat(Lat2,Val2),
    Val1<Val2,
    !.

Lat1 is_at_or_south_of Lat2:-
    the_total_Val_in_arc_mins_of_lat(Lat1,Val1),
    the_total_Val_in_arc_mins_of_lat(Lat2,Val2),
    Val1=<Val2,
    !.

same_Long(Long1,Long2):-
    the_total_Val_in_arc_mins_of_long(Long1,Val1),
    the_total_Val_in_arc_mins_of_long(Long2,Val2),
    Val1=Val2,
    !.

the_total_Val_in_arc_mins_of_long(long_E(Int_0_to_180),Val1):-
    Val1 is Int_0_to_180*60,
    !.

the_total_Val_in_arc_mins_of_long(long_W(Int_0_to_180),Val1):-
    Val1 is(21600-Int_0_to_180*60)mod 21600,
    !.

the_total_Val_in_arc_mins_of_long(long_E(Int_0_to_179,Int_0_to_59),Val1):-
    Val1 is Int_0_to_179*60+Int_0_to_59,
    !.

the_total_Val_in_arc_mins_of_long(long_W(Int_0_to_179,Int_0_to_59),Val1):-
    Val1 is (21600 - (Int_0_to_179*60 - Int_0_to_59)) mod 21600,
    !.

the_arc_mins_eastward_Val_from(Long1,Long2,Val3):-
    the_total_Val_in_arc_mins_of_long(Long2,Val1),
    the_total_Val_in_arc_mins_of_long(Long1,Val2),
    Val3 is max(Val1-Val2,(Val1+(21600-Val2))mod 21600),
    !.

the_arc_mins_westward_Val_from(Long1,Long2,Val1):-
    the_arc_mins_eastward_Val_from(Long2,Long1,Val1),
    !.

Long2 is_east_of Long1:-
    ground_not__((same_Long(Long2,Long1))),
    the_arc_mins_eastward_Val_from(Long1,Long2,Val1),
    the_arc_mins_westward_Val_from(Long1,Long2,Val2),
    Val1=<Val2,
    !.

Long2 is_west_of Long1:-
    Long1 is_east_of Long2,
    !.

Long2 is_at_or_west_of Long1:-
    ( same_Long(Long2,Long1)
      ;
      Long2 is_west_of Long1
    ),
    !.

same_Flight_level(Flight_level1,Flight_level2):-
    the_Val_in_100s_of_feet_of(Flight_level1,Val1),
    the_Val_in_100s_of_feet_of(Flight_level2,Val2),
    Val1=Val2,
    !.

the_Val_in_100s_of_feet_of(fl(Int_0_to_1000),Int_0_to_1000).

the_Val_in_feet_of(fl(Int_0_to_1000),Val1):-
    Val1 is 100*Int_0_to_1000,
    !.

Flight_level1 is_at_or_above Flight_level2:-
    the_Val_in_100s_of_feet_of(Flight_level1,Val1),
    the_Val_in_100s_of_feet_of(Flight_level2,Val2),
    Val1>=Val2,
    !.

Flight_level1 is_at_or_below Flight_level2:-
    the_Val_in_100s_of_feet_of(Flight_level1,Val1),
    the_Val_in_100s_of_feet_of(Flight_level2,Val2),
    Val1=<Val2,
    !.

Flight_level1 is_above Flight_level2:-
    the_Val_in_100s_of_feet_of(Flight_level1,Val1),
    the_Val_in_100s_of_feet_of(Flight_level2,Val2),
    Val1>Val2,
    !.

Flight_level1 is_below Flight_level2:-
    the_Val_in_100s_of_feet_of(Flight_level1,Val1),
    the_Val_in_100s_of_feet_of(Flight_level2,Val2),
    Val1<Val2,
    !.

time_of_current_conflict_probe(time(0,1,0)).

same_Time(Time1,Time2):-
    the_total_Val_in_mins_of(Time1,Val1),
    the_total_Val_in_mins_of(Time2,Val2),
    Val1=Val2,
    !.

non_negative_integer(Val1):-
    the_total_Val_in_mins_of(Time,Val1),
    !.

the_total_Val_in_mins_of(time(Int_0_to_23,Int_0_to_59,Int_gte_0),Val1):-
    Val1 is Int_gte_0*1440+(Int_0_to_23*60+Int_0_to_59),
    !.

Time1 is_at_or_later_than Time2:-
    the_total_Val_in_mins_of(Time1,Val1),
    the_total_Val_in_mins_of(Time2,Val2),
    Val1>=Val2,
    !.

Time1 is_at_or_earlier_than Time2:-
    the_total_Val_in_mins_of(Time1,Val1),
    the_total_Val_in_mins_of(Time2,Val2),
    Val1=<Val2,
    !.

Time1 is_earlier_than Time2:-
    the_total_Val_in_mins_of(Time1,Val1),
    the_total_Val_in_mins_of(Time2,Val2),
    Val1<Val2,
    !.

Time1 is_later_than Time2:-
    the_total_Val_in_mins_of(Time1,Val1),
    the_total_Val_in_mins_of(Time2,Val2),
    Val1>Val2,
    !.

the_next_integer_Time_in_mins_after(time(Int_0_to_23,Int_0_to_59,Int_gte_0),time(Int_0_to_23,Int_0_to_59_,Int_gte_0)):-
    Int_0_to_59<59,
    Val1 is Int_0_to_59+1,
    Int_0_to_59_=Val1,
    !.

the_next_integer_Time_in_mins_after(time(Int_0_to_23,Int_0_to_59,Int_gte_0),time(Int_0_to_23_,0,Int_gte_0)):-
    Int_0_to_59=59,
    Int_0_to_23<23,
    Val1 is Int_0_to_23+1,
    Int_0_to_23_=Val1,
    !.

the_next_integer_Time_in_mins_after(time(Int_0_to_23,Int_0_to_59,Int_gte_0),time(0,0,Int_gte_0_)):-
    Int_0_to_59=59,
    Int_0_to_23=23,
    Val1 is Int_gte_0+1,
    Int_gte_0_=Val1,
    !.

same_Flight_level_range(Flight_level_range1,Flight_level_range2):-
    the_max_Flight_level_of_fl_range(Flight_level_range1,Flight_level1),
    the_max_Flight_level_of_fl_range(Flight_level_range2,Flight_level2),
    same_Flight_level(Flight_level1,Flight_level2),
    the_min_Flight_level_of_fl_range(Flight_level_range1,Flight_level3),
    the_min_Flight_level_of_fl_range(Flight_level_range2,Flight_level4),
    same_Flight_level(Flight_level3,Flight_level4),
    !.

the_max_Flight_level_of_fl_range(fl_range(Flight_level1,Flight_level2),Flight_level1):-
    Flight_level1 is_at_or_above Flight_level2,
    !.

the_max_Flight_level_of_fl_range(fl_range(Flight_level1,Flight_level2),Flight_level2):-
    Flight_level2 is_at_or_above Flight_level1,
    !.

the_min_Flight_level_of_fl_range(fl_range(Flight_level1,Flight_level2),Flight_level1):-
    Flight_level1 is_at_or_below Flight_level2,
    !.

the_min_Flight_level_of_fl_range(fl_range(Flight_level1,Flight_level2),Flight_level2):-
    Flight_level2 is_at_or_below Flight_level1,
    !.

same_2D_pt(Two_D_pt1,Two_D_pt2):-
    the_Lat_of_2D_pt(Two_D_pt1,Lat1),
    the_Lat_of_2D_pt(Two_D_pt2,Lat2),
    same_Lat(Lat1,Lat2),
    the_Long_of_2D_pt(Two_D_pt1,Long1),
    the_Long_of_2D_pt(Two_D_pt2,Long2),
    same_Long(Long1,Long2),
    !.

the_Lat_of_2D_pt(twoD_pt(Lat,Long),Lat).

the_Long_of_2D_pt(twoD_pt(Lat,Long),Long).

the_nm_linear_dist_Val_between_2D_pts(Two_D_pt1,Two_D_pt2,Val6):-
    the_lat_Val_in_arc_mins_of(Two_D_pt1,Val1),
    the_lat_Val_in_arc_mins_of(Two_D_pt2,Val2),
    the_long_Val_in_arc_mins_of(Two_D_pt1,Val3),
    the_long_Val_in_arc_mins_of(Two_D_pt2,Val4),
    the_long_arc_mins_dist_Val_between(Val3,Val4,Val5),
    Val6 is sqrt(exp(Val1-Val2,2)+exp(Val5,2)),
    !.

the_lat_Val_in_arc_mins_of(Two_D_pt,Val1):-
    the_Lat_of_2D_pt(Two_D_pt,Lat1),
    the_total_Val_in_arc_mins_of_lat(Lat1,Val1),
    !.

the_long_Val_in_arc_mins_of(Two_D_pt,Val1):-
    the_Long_of_2D_pt(Two_D_pt,Long1),
    the_total_Val_in_arc_mins_of_long(Long1,Val1),
    !.

same_3D_pt(Three_D_pt1,Three_D_pt2):-
    the_2D_pt_of_3D_pt(Three_D_pt1,Two_D_pt1),
    the_2D_pt_of_3D_pt(Three_D_pt2,Two_D_pt2),
    same_2D_pt(Two_D_pt1,Two_D_pt2),
    the_Flight_level_range_of_3D_pt(Three_D_pt1,Flight_level_range1),
    the_Flight_level_range_of_3D_pt(Three_D_pt2,Flight_level_range2),
    same_Flight_level_range(Flight_level_range1,Flight_level_range2),
    !.

the_2D_pt_of_3D_pt(threeD_pt(Two_D_pt,Flight_level_range),Two_D_pt).

the_Flight_level_range_of_3D_pt(threeD_pt(Two_D_pt,Flight_level_range),Flight_level_range).

same_4D_pt(Four_D_pt1,Four_D_pt2):-
    the_3D_pt_of(Four_D_pt1,Three_D_pt1),
    the_3D_pt_of(Four_D_pt2,Three_D_pt2),
    same_3D_pt(Three_D_pt1,Three_D_pt2),
    the_Time_of(Four_D_pt1,Time1),
    the_Time_of(Four_D_pt2,Time2),
    same_Time(Time1,Time2),
    !.

the_3D_pt_of(fourD_pt(Three_D_pt,Time),Three_D_pt).

the_Time_of(fourD_pt(Three_D_pt,Time),Time).

the_2D_pt_of_4D_pt(Four_D_pt,Two_D_pt1):-
    the_3D_pt_of(Four_D_pt,Three_D_pt1),
    the_2D_pt_of_3D_pt(Three_D_pt1,Two_D_pt1),
    !.

the_Flight_level_range_of_4D_pt(Four_D_pt,Flight_level_range1):-
    the_3D_pt_of(Four_D_pt,Three_D_pt1),
    the_Flight_level_range_of_3D_pt(Three_D_pt1,Flight_level_range1),
    !.

the_Lat_of_4D_pt(Four_D_pt,Lat1):-
    the_2D_pt_of_4D_pt(Four_D_pt,Two_D_pt1),
    the_Lat_of_2D_pt(Two_D_pt1,Lat1),
    !.

the_Long_of_4D_pt(Four_D_pt,Long1):-
    the_2D_pt_of_4D_pt(Four_D_pt,Two_D_pt1),
    the_Long_of_2D_pt(Two_D_pt1,Long1),
    !.

the_entry_Segment_of(Profile,Segment):-
    Segment belongs_to Profile,
    not__((Segment1 belongs_to Profile,
        the_entry_Time_of(Segment1,Time1),
        the_entry_Time_of(Segment,Time2),
        Time1 is_earlier_than Time2)),
    !.

the_first_4D_pt_of(Profile,Four_D_pt1):-
    the_entry_Segment_of(Profile,Segment1),
    the_entry_4D_pt_of(Segment1,Four_D_pt1),
    !.

is_wholly_or_partly_in_the_mnps_airspace(Profile):-
    Segment belongs_to Profile,
    ( the_entry_4D_pt_of(Segment,Four_D_pt1),
      the_2D_pt_of_4D_pt(Four_D_pt1,Two_D_pt1),
      is_within_lateral_and_longitudinal_boundaries_of_airspace(Two_D_pt1,mnps),
      the_entry_Flight_level_range_of(Segment,Flight_level_range1),
      overlaps_with_the_flight_level_range_of_airspace(Flight_level_range1,mnps)
      ;
      the_exit_4D_pt_of(Segment,Four_D_pt2),
      the_2D_pt_of_4D_pt(Four_D_pt2,Two_D_pt2),
      is_within_lateral_and_longitudinal_boundaries_of_airspace(Two_D_pt2,mnps),
      the_exit_Flight_level_range_of(Segment,Flight_level_range2),
      overlaps_with_the_flight_level_range_of_airspace(Flight_level_range2,mnps)
    ),
    !.

is_wholly_outside_the_mnps_airspace(Profile):-
    ground_not__((is_wholly_or_partly_in_the_mnps_airspace(Profile))),
    !.

is_wholly_within_the_mnps_airspace(Profile):-
    not__((Segment belongs_to Profile,
        ( the_entry_4D_pt_of(Segment,Four_D_pt1),
          the_2D_pt_of_4D_pt(Four_D_pt1,Two_D_pt1),
          ground_not__((is_within_lateral_and_longitudinal_boundaries_of_airspace(Two_D_pt1,mnps)))
          ;
          ( the_exit_4D_pt_of(Segment,Four_D_pt2),
            the_2D_pt_of_4D_pt(Four_D_pt2,Two_D_pt2),
            ground_not__((is_within_lateral_and_longitudinal_boundaries_of_airspace(Two_D_pt2,mnps)))
            ;
            ( the_entry_Flight_level_range_of(Segment,Flight_level_range1),
              ground_not__((overlaps_with_the_flight_level_range_of_airspace(Flight_level_range1,mnps)))
              ;
              the_exit_Flight_level_range_of(Segment,Flight_level_range2),
              ground_not__((overlaps_with_the_flight_level_range_of_airspace(Flight_level_range2,mnps)))
            )
          )
        ))),
    !.

is_wholly_or_partly_in_oca(Profile):-
    Segment belongs_to Profile,
    ( the_entry_4D_pt_of(Segment,Four_D_pt1),
      the_2D_pt_of_4D_pt(Four_D_pt1,Two_D_pt1),
      is_within_lateral_and_longitudinal_boundaries_of_airspace(Two_D_pt1,oceanic),
      the_entry_Flight_level_range_of(Segment,Flight_level_range1),
      overlaps_with_the_flight_level_range_of_airspace(Flight_level_range1,oceanic)
      ;
      the_exit_4D_pt_of(Segment,Four_D_pt2),
      the_2D_pt_of_4D_pt(Four_D_pt2,Two_D_pt2),
      is_within_lateral_and_longitudinal_boundaries_of_airspace(Two_D_pt2,oceanic),
      the_exit_Flight_level_range_of(Segment,Flight_level_range2),
      overlaps_with_the_flight_level_range_of_airspace(Flight_level_range2,oceanic)
    ),
    !.

the_first_recognised_4D_pt_for_oceanic_cpr_of(Profile,Four_D_pt1):-
    Segment belongs_to Profile,
    the_entry_4D_pt_of(Segment,Four_D_pt1),
    the_2D_pt_of_4D_pt(Four_D_pt1,Two_D_pt1),
    is_within_lateral_and_longitudinal_boundaries_of_airspace(Two_D_pt1,oceanic),
    not__((Segment1 belongs_to Profile,
        the_entry_4D_pt_of(Segment1,Four_D_pt2),
        the_2D_pt_of_4D_pt(Four_D_pt2,Two_D_pt2),
        is_within_lateral_and_longitudinal_boundaries_of_airspace(Two_D_pt2,oceanic),
        the_entry_Time_of(Segment1,Time1),
        the_entry_Time_of(Segment,Time2),
        Time1 is_earlier_than Time2)),
    !.

the_last_recognised_4D_pt_for_oceanic_cpr_of(Profile,Four_D_pt1):-
    Segment belongs_to Profile,
    the_exit_4D_pt_of(Segment,Four_D_pt1),
    the_2D_pt_of_4D_pt(Four_D_pt1,Two_D_pt1),
    is_within_lateral_and_longitudinal_boundaries_of_airspace(Two_D_pt1,oceanic),
    not__((Segment1 belongs_to Profile,
        the_exit_4D_pt_of(Segment1,Four_D_pt2),
        the_2D_pt_of_4D_pt(Four_D_pt2,Two_D_pt2),
        is_within_lateral_and_longitudinal_boundaries_of_airspace(Two_D_pt2,oceanic),
        the_exit_Time_of(Segment1,Time1),
        the_exit_Time_of(Segment,Time2),
        Time1 is_later_than Time2)),
    !.

the_exit_Segment_of(Profile,Segment):-
    Segment belongs_to Profile,
    not__((Segment1 belongs_to Profile,
        the_entry_Time_of(Segment1,Time1),
        the_entry_Time_of(Segment,Time2),
        Time1 is_later_than Time2)),
    !.

same_Segment(Segment1,Segment2):-
    the_entry_4D_pt_of(Segment1,Four_D_pt1),
    the_entry_4D_pt_of(Segment2,Four_D_pt2),
    same_4D_pt(Four_D_pt1,Four_D_pt2),
    the_exit_4D_pt_of(Segment1,Four_D_pt3),
    the_exit_4D_pt_of(Segment2,Four_D_pt4),
    same_4D_pt(Four_D_pt3,Four_D_pt4),
    the_machno_Val_on(Segment1,Val1),
    the_machno_Val_on(Segment2,Val2),
    Val1=Val2,
    the_Profile_containing(Segment1,Profile1),
    the_Profile_containing(Segment2,Profile2),
    Profile1=Profile2,
    !.

the_Profile_containing(the_Segment(Profile,Four_D_pt1,Four_D_pt2,Val),Profile).

the_entry_4D_pt_of(the_Segment(Profile,Four_D_pt1,Four_D_pt2,Val),Four_D_pt1).

the_exit_4D_pt_of(the_Segment(Profile,Four_D_pt1,Four_D_pt2,Val),Four_D_pt2).

the_machno_Val_on(the_Segment(Profile,Four_D_pt1,Four_D_pt2,Val),Val1):-
    Val1 is integer(100*Val),
    !.

time_periods_overlap(Segment1,Segment2):-
    ( the_entry_Time_of(Segment1,Time1),
      Time1 is_during_time_period_of Segment2
      ;
      the_entry_Time_of(Segment2,Time2),
      Time2 is_during_time_period_of Segment1
    ),
    !.

flight_level_ranges_overlap(Segment1,Segment2):-
    ( the_max_Flight_level_of_segment(Segment2,Flight_level1),
      Flight_level1 lies_in_flight_level_range_of Segment1
      ;
      the_max_Flight_level_of_segment(Segment1,Flight_level2),
      Flight_level2 lies_in_flight_level_range_of Segment2
    ),
    !.

Flight_level lies_in_flight_level_range_of Segment:-
    the_min_Flight_level_of_segment(Segment,Flight_level1),
    Flight_level is_at_or_above Flight_level1,
    the_max_Flight_level_of_segment(Segment,Flight_level2),
    Flight_level is_at_or_below Flight_level2,
    !.

the_entry_Time_of(Segment,Time1):-
    the_entry_4D_pt_of(Segment,Four_D_pt1),
    the_Time_of(Four_D_pt1,Time1),
    !.

the_exit_Time_of(Segment,Time1):-
    the_exit_4D_pt_of(Segment,Four_D_pt1),
    the_Time_of(Four_D_pt1,Time1),
    !.

belong_to_profiles_for_different_aircraft(Segment1,Segment2):-
    the_Aircraft_on_segment(Segment1,Aircraft1),
    the_Aircraft_on_segment(Segment2,Aircraft2),
    ground_not__((Aircraft1=Aircraft2)),
    !.

the_entry_Flight_level_range_of(Segment,Flight_level_range1):-
    the_entry_4D_pt_of(Segment,Four_D_pt1),
    the_Flight_level_range_of_4D_pt(Four_D_pt1,Flight_level_range1),
    !.

the_exit_Flight_level_range_of(Segment,Flight_level_range1):-
    the_exit_4D_pt_of(Segment,Four_D_pt1),
    the_Flight_level_range_of_4D_pt(Four_D_pt1,Flight_level_range1),
    !.

one_or_both_start_later_than(Segment1,Segment2,Time):-
    ( the_entry_Time_of(Segment1,Time1),
      Time1 is_later_than Time
      ;
      the_entry_Time_of(Segment2,Time2),
      Time2 is_later_than Time
    ),
    !.

is_westerly(Segment):-
    the_exit_Long_of(Segment,Long1),
    the_entry_Long_of(Segment,Long2),
    Long1 is_west_of Long2,
    !.

is_easterly(Segment):-
    the_exit_Long_of(Segment,Long1),
    the_entry_Long_of(Segment,Long2),
    Long1 is_east_of Long2,
    !.

the_entry_Lat_of(Segment,Lat1):-
    the_entry_4D_pt_of(Segment,Four_D_pt1),
    the_Lat_of_4D_pt(Four_D_pt1,Lat1),
    !.

the_exit_Lat_of(Segment,Lat1):-
    the_exit_4D_pt_of(Segment,Four_D_pt1),
    the_Lat_of_4D_pt(Four_D_pt1,Lat1),
    !.

the_entry_Long_of(Segment,Long1):-
    the_entry_4D_pt_of(Segment,Four_D_pt1),
    the_Long_of_4D_pt(Four_D_pt1,Long1),
    !.

the_exit_Long_of(Segment,Long1):-
    the_exit_4D_pt_of(Segment,Four_D_pt1),
    the_Long_of_4D_pt(Four_D_pt1,Long1),
    !.

the_most_northerly_Lat_of(Segment,Lat1):-
    the_exit_Lat_of(Segment,Lat1),
    the_entry_Lat_of(Segment,Lat2),
    Lat1 is_at_or_north_of Lat2,
    !.

the_most_northerly_Lat_of(Segment,Lat1):-
    the_entry_Lat_of(Segment,Lat1),
    the_exit_Lat_of(Segment,Lat2),
    Lat2 is_south_of Lat1,
    !.

the_most_southerly_Lat_of(Segment,Lat1):-
    the_exit_Lat_of(Segment,Lat1),
    the_entry_Lat_of(Segment,Lat2),
    Lat1 is_at_or_south_of Lat2,
    !.

the_most_southerly_Lat_of(Segment,Lat1):-
    the_entry_Lat_of(Segment,Lat1),
    the_exit_Lat_of(Segment,Lat2),
    Lat2 is_north_of Lat1,
    !.

the_max_Flight_level_of_segment(Segment,Flight_level1):-
    the_max_exit_Flight_level_of(Segment,Flight_level1),
    the_max_entry_Flight_level_of(Segment,Flight_level2),
    Flight_level1 is_at_or_above Flight_level2,
    !.

the_max_Flight_level_of_segment(Segment,Flight_level1):-
    the_max_entry_Flight_level_of(Segment,Flight_level1),
    the_max_exit_Flight_level_of(Segment,Flight_level2),
    Flight_level2 is_below Flight_level1,
    !.

the_min_Flight_level_of_segment(Segment,Flight_level1):-
    the_min_exit_Flight_level_of(Segment,Flight_level1),
    the_min_entry_Flight_level_of(Segment,Flight_level2),
    Flight_level1 is_at_or_below Flight_level2,
    !.

the_min_Flight_level_of_segment(Segment,Flight_level1):-
    the_min_entry_Flight_level_of(Segment,Flight_level1),
    the_min_exit_Flight_level_of(Segment,Flight_level2),
    Flight_level2 is_above Flight_level1,
    !.

the_lat_over_long_gradient_Val_of(Segment,Val4):-
    the_exit_Long_of(Segment,Long1),
    the_entry_Long_of(Segment,Long2),
    Long1 is_east_of Long2,
    the_exit_lat_Val_in_arc_mins_of(Segment,Val1),
    the_entry_lat_Val_in_arc_mins_of(Segment,Val2),
    the_long_length_Val_in_arc_mins_of(Segment,Val3),
    Val4 is(Val1-Val2)/Val3,
    !.

the_lat_over_long_gradient_Val_of(Segment,Val4):-
    the_exit_Long_of(Segment,Long1),
    the_entry_Long_of(Segment,Long2),
    Long1 is_west_of Long2,
    the_exit_lat_Val_in_arc_mins_of(Segment,Val1),
    the_entry_lat_Val_in_arc_mins_of(Segment,Val2),
    the_long_length_Val_in_arc_mins_of(Segment,Val3),
    Val4 is -(Val1-Val2)/Val3,
    !.

the_most_westerly_Long_of(Segment,Long1):-
    the_exit_Long_of(Segment,Long1),
    the_entry_Long_of(Segment,Long2),
    Long1 is_at_or_west_of Long2,
    !.

the_most_westerly_Long_of(Segment,Long1):-
    the_entry_Long_of(Segment,Long1),
    the_exit_Long_of(Segment,Long2),
    Long2 is_east_of Long1,
    !.

the_most_easterly_Long_of(Segment,Long1):-
    the_exit_Long_of(Segment,Long1),
    the_entry_Long_of(Segment,Long2),
    Long1 is_east_of Long2,
    !.

the_most_easterly_Long_of(Segment,Long1):-
    the_entry_Long_of(Segment,Long1),
    the_exit_Long_of(Segment,Long2),
    Long2 is_at_or_west_of Long1,
    !.

finishes_at_or_earlier_than_starts(Segment1,Segment2):-
    the_entry_Time_of(Segment2,Time1),
    the_exit_Time_of(Segment1,Time2),
    Time1 is_at_or_later_than Time2,
    !.

the_entry_Linear_track_pt_of(Segment,the_Linear_track_pt(Segment,Val1)):-
    the_entry_time_Val_in_mins_of(Segment,Val1),
    !.

the_exit_Linear_track_pt_of(Segment,the_Linear_track_pt(Segment,Val1)):-
    the_exit_time_Val_in_mins_of(Segment,Val1),
    !.

the_nm_linear_track_length_Val_of(Segment,Val3):-
    the_lat_length_Val_in_arc_mins_of(Segment,Val1),
    the_long_length_Val_in_arc_mins_of(Segment,Val2),
    Val3 is sqrt(exp(Val1,2)+exp(Val2,2)),
    !.

the_lat_length_Val_in_arc_mins_of(Segment,Val3):-
    the_exit_Lat_of(Segment,Lat1),
    the_total_Val_in_arc_mins_of_lat(Lat1,Val1),
    the_entry_Lat_of(Segment,Lat2),
    the_total_Val_in_arc_mins_of_lat(Lat2,Val2),
    Val3 is abs(Val1-Val2),
    !.

the_long_length_Val_in_arc_mins_of(Segment,Val3):-
    the_entry_Long_of(Segment,Long1),
    the_exit_Long_of(Segment,Long2),
    the_arc_mins_eastward_Val_from(Long1,Long2,Val1),
    the_arc_mins_westward_Val_from(Long1,Long2,Val2),
    Val3 is min(Val1,Val2),
    !.

regular(Segment):-
    the_exit_Long_of(Segment,Long1),
    the_entry_Long_of(Segment,Long2),
    ground_not__((same_Long(Long1,Long2))),
    the_lat_length_Val_in_arc_mins_of(Segment,Val1),
    the_long_length_Val_in_arc_mins_of(Segment,Val2),
    the_average_lat_Val_in_arc_mins_of(Segment,Val3),
    Val4 is Val1/(Val2*cos(Val3*0.0002909)),
    Val4<1,
    !.

irregular(Segment):-
    ground_not__((regular(Segment))),
    !.

is_cruise_climbed(Segment):-
    ( the_max_entry_Flight_level_of(Segment,Flight_level1),
      the_min_entry_Flight_level_of(Segment,Flight_level2),
      Flight_level1 is_above Flight_level2
      ;
      the_max_exit_Flight_level_of(Segment,Flight_level3),
      the_min_exit_Flight_level_of(Segment,Flight_level4),
      Flight_level3 is_above Flight_level4
    ),
    !.

Time is_during_time_period_of Segment:-
    the_entry_Time_of(Segment,Time1),
    Time is_at_or_later_than Time1,
    the_exit_Time_of(Segment,Time2),
    Time is_at_or_earlier_than Time2,
    !.

the_offset_const_Val_for_linear_track_of(Segment,Val4):-
    the_entry_Long_of(Segment,Long1),
    the_exit_Long_of(Segment,Long2),
    ground_not__((same_Long(Long1,Long2))),
    the_entry_lat_Val_in_arc_mins_of(Segment,Val1),
    the_lat_over_long_gradient_Val_of(Segment,Val2),
    the_entry_long_Val_in_arc_mins_of(Segment,Val3),
    Val4 is Val1-Val2*Val3,
    !.

the_average_lat_Val_in_arc_mins_of(Segment,Val3):-
    the_entry_lat_Val_in_arc_mins_of(Segment,Val1),
    the_exit_lat_Val_in_arc_mins_of(Segment,Val2),
    Val3 is(Val1+Val2)/2,
    !.

the_entry_lat_Val_in_arc_mins_of(Segment,Val1):-
    the_entry_Lat_of(Segment,Lat1),
    the_total_Val_in_arc_mins_of_lat(Lat1,Val1),
    !.

the_exit_lat_Val_in_arc_mins_of(Segment,Val1):-
    the_exit_Lat_of(Segment,Lat1),
    the_total_Val_in_arc_mins_of_lat(Lat1,Val1),
    !.

the_entry_time_Val_in_mins_of(Segment,Val1):-
    the_entry_Time_of(Segment,Time1),
    the_total_Val_in_mins_of(Time1,Val1),
    !.

the_exit_time_Val_in_mins_of(Segment,Val1):-
    the_exit_Time_of(Segment,Time1),
    the_total_Val_in_mins_of(Time1,Val1),
    !.

the_entry_long_Val_in_arc_mins_of(Segment,Val1):-
    the_entry_Long_of(Segment,Long1),
    the_total_Val_in_arc_mins_of_long(Long1,Val1),
    !.

the_exit_long_Val_in_arc_mins_of(Segment,Val1):-
    the_exit_Long_of(Segment,Long1),
    the_total_Val_in_arc_mins_of_long(Long1,Val1),
    !.

the_time_to_fly_Val_in_mins_of(Segment,Val3):-
    the_exit_Time_of(Segment,Time1),
    the_total_Val_in_mins_of(Time1,Val1),
    the_entry_Time_of(Segment,Time2),
    the_total_Val_in_mins_of(Time2,Val2),
    Val3 is Val1-Val2,
    !.

the_Aircraft_on_segment(Segment,Aircraft1):-
    the_Profile_containing(Segment,Profile1),
    the_Aircraft_on_profile(Profile1,Aircraft1),
    !.

the_entry_2D_pt_of(Segment,Two_D_pt1):-
    the_entry_4D_pt_of(Segment,Four_D_pt1),
    the_2D_pt_of_4D_pt(Four_D_pt1,Two_D_pt1),
    !.

the_exit_2D_pt_of(Segment,Two_D_pt1):-
    the_exit_4D_pt_of(Segment,Four_D_pt1),
    the_2D_pt_of_4D_pt(Four_D_pt1,Two_D_pt1),
    !.

the_earlier_Segment_of(Segment1,Segment2,Segment1):-
    ground_not__((time_periods_overlap(Segment1,Segment2))),
    the_exit_Time_of(Segment1,Time1),
    the_entry_Time_of(Segment2,Time2),
    Time1 is_earlier_than Time2,
    !.

the_earlier_Segment_of(Segment1,Segment2,Segment2):-
    ground_not__((time_periods_overlap(Segment1,Segment2))),
    the_exit_Time_of(Segment2,Time1),
    the_entry_Time_of(Segment1,Time2),
    Time1 is_earlier_than Time2,
    !.

the_later_Segment_of(Segment1,Segment2,Segment1):-
    ground_not__((time_periods_overlap(Segment1,Segment2))),
    the_entry_Time_of(Segment1,Time1),
    the_exit_Time_of(Segment2,Time2),
    Time1 is_later_than Time2,
    !.

the_later_Segment_of(Segment1,Segment2,Segment2):-
    ground_not__((time_periods_overlap(Segment1,Segment2))),
    the_entry_Time_of(Segment2,Time1),
    the_exit_Time_of(Segment1,Time2),
    Time1 is_later_than Time2,
    !.

the_max_entry_Flight_level_of(Segment,Flight_level1):-
    the_entry_Flight_level_range_of(Segment,Flight_level_range1),
    the_max_Flight_level_of_fl_range(Flight_level_range1,Flight_level1),
    !.

the_min_entry_Flight_level_of(Segment,Flight_level1):-
    the_entry_Flight_level_range_of(Segment,Flight_level_range1),
    the_min_Flight_level_of_fl_range(Flight_level_range1,Flight_level1),
    !.

the_max_exit_Flight_level_of(Segment,Flight_level1):-
    the_exit_Flight_level_range_of(Segment,Flight_level_range1),
    the_max_Flight_level_of_fl_range(Flight_level_range1,Flight_level1),
    !.

the_min_exit_Flight_level_of(Segment,Flight_level1):-
    the_exit_Flight_level_range_of(Segment,Flight_level_range1),
    the_min_Flight_level_of_fl_range(Flight_level_range1,Flight_level1),
    !.

same_Linear_track_pt(Linear_track_pt1,Linear_track_pt2):-
    the_Segment_of(Linear_track_pt1,Segment1),
    the_Segment_of(Linear_track_pt2,Segment2),
    same_Segment(Segment1,Segment2),
    the_total_time_Val_in_mins_at(Linear_track_pt1,Val1),
    the_total_time_Val_in_mins_at(Linear_track_pt2,Val2),
    Val1=Val2,
    !.

the_Segment_of(the_Linear_track_pt(Segment,Val),Segment).

the_total_time_Val_in_mins_at(the_Linear_track_pt(Segment,Val),Val).

the_lat_Val_in_arc_mins_of(Linear_track_pt,Val6):-
    the_segment_entry_lat_Val_in_arc_mins_for(Linear_track_pt,Val1),
    the_segment_exit_lat_Val_in_arc_mins_for(Linear_track_pt,Val2),
    the_total_time_Val_in_mins_at(Linear_track_pt,Val3),
    the_segment_entry_time_Val_in_mins_for(Linear_track_pt,Val4),
    the_segment_exit_time_Val_in_mins_for(Linear_track_pt,Val5),
    Val6 is round(Val1+(Val2-Val1)*((Val3-Val4)/(Val5-Val4))),
    !.

the_long_Val_in_arc_mins_of(Linear_track_pt,Val1):-
    the_segment_entry_long_Val_in_arc_mins_for(Linear_track_pt,Val1),
    the_segment_exit_long_Val_in_arc_mins_for(Linear_track_pt,Val2),
    Val1=Val2,
    !.

the_long_Val_in_arc_mins_of(Linear_track_pt,Val6):-
    the_Segment_of(Linear_track_pt,Segment1),
    is_westerly(Segment1),
    the_segment_entry_long_Val_in_arc_mins_for(Linear_track_pt,Val1),
    the_long_length_Val_in_arc_mins_of(Segment1,Val2),
    the_total_time_Val_in_mins_at(Linear_track_pt,Val3),
    the_segment_entry_time_Val_in_mins_for(Linear_track_pt,Val4),
    the_segment_exit_time_Val_in_mins_for(Linear_track_pt,Val5),
    Val6 is round(Val1+(21600-Val2*((Val3-Val4)/(Val5-Val4))))mod 21600,
    !.

the_long_Val_in_arc_mins_of(Linear_track_pt,Val6):-
    the_Segment_of(Linear_track_pt,Segment1),
    is_easterly(Segment1),
    the_segment_entry_long_Val_in_arc_mins_for(Linear_track_pt,Val1),
    the_long_length_Val_in_arc_mins_of(Segment1,Val2),
    the_total_time_Val_in_mins_at(Linear_track_pt,Val3),
    the_segment_entry_time_Val_in_mins_for(Linear_track_pt,Val4),
    the_segment_exit_time_Val_in_mins_for(Linear_track_pt,Val5),
    Val6 is round(Val1+(21600+Val2*((Val3-Val4)/(Val5-Val4))))mod 21600,
    !.

the_segment_entry_lat_Val_in_arc_mins_for(Linear_track_pt,Val1):-
    the_Segment_of(Linear_track_pt,Segment1),
    the_entry_lat_Val_in_arc_mins_of(Segment1,Val1),
    !.

the_segment_exit_lat_Val_in_arc_mins_for(Linear_track_pt,Val1):-
    the_Segment_of(Linear_track_pt,Segment1),
    the_exit_lat_Val_in_arc_mins_of(Segment1,Val1),
    !.

the_segment_entry_long_Val_in_arc_mins_for(Linear_track_pt,Val1):-
    the_Segment_of(Linear_track_pt,Segment1),
    the_entry_long_Val_in_arc_mins_of(Segment1,Val1),
    !.

the_segment_exit_long_Val_in_arc_mins_for(Linear_track_pt,Val1):-
    the_Segment_of(Linear_track_pt,Segment1),
    the_exit_long_Val_in_arc_mins_of(Segment1,Val1),
    !.

the_nm_linear_dist_Val_between_linear_track_pts(Linear_track_pt1,Linear_track_pt2,Val6):-
    the_lat_Val_in_arc_mins_of(Linear_track_pt1,Val1),
    the_lat_Val_in_arc_mins_of(Linear_track_pt2,Val2),
    the_long_Val_in_arc_mins_of(Linear_track_pt1,Val3),
    the_long_Val_in_arc_mins_of(Linear_track_pt2,Val4),
    the_long_arc_mins_dist_Val_between(Val3,Val4,Val5),
    Val6 is sqrt(exp(Val1-Val2,2)+exp(Val5,2)),
    !.

the_segment_entry_time_Val_in_mins_for(Linear_track_pt,Val1):-
    the_Segment_of(Linear_track_pt,Segment1),
    the_entry_time_Val_in_mins_of(Segment1,Val1),
    !.

the_segment_exit_time_Val_in_mins_for(Linear_track_pt,Val1):-
    the_Segment_of(Linear_track_pt,Segment1),
    the_exit_time_Val_in_mins_of(Segment1,Val1),
    !.

same_Airspace(Airspace1,Airspace2):-
    not__((are_vertices_for(Two_D_pt1,Two_D_pt2,Airspace1),
        ( ground_not__((are_vertices_for(Two_D_pt1,Two_D_pt2,Airspace2)))
          ;
          ground_not__((are_vertices_for(Two_D_pt2,Two_D_pt1,Airspace2)))
        ))),
    not__((are_vertices_for(Two_D_pt1,Two_D_pt2,Airspace2),
        ( ground_not__((are_vertices_for(Two_D_pt1,Two_D_pt2,Airspace1)))
          ;
          ground_not__((are_vertices_for(Two_D_pt2,Two_D_pt1,Airspace1)))
        ))),
    !.

overlaps_with_the_flight_level_range_of_airspace(Flight_level_range,Airspace):-
    is_the_max_flight_level_for(Flight_level1,Airspace),
    is_the_min_flight_level_for(Flight_level2,Airspace),
    the_min_Flight_level_of_fl_range(Flight_level_range,Flight_level3),
    Flight_level3 is_at_or_below Flight_level1,
    the_max_Flight_level_of_fl_range(Flight_level_range,Flight_level4),
    Flight_level4 is_at_or_above Flight_level2,
    !.

is_within_lateral_and_longitudinal_boundaries_of_airspace(Two_D_pt,shanwick):-
    ( the_Long_of_2D_pt(Two_D_pt,Long1),
      long_W(30)is_at_or_west_of Long1,
      the_Long_of_2D_pt(Two_D_pt,Long1),
      Long1 is_at_or_west_of long_W(15),
      the_Lat_of_2D_pt(Two_D_pt,Lat1),
      lat_N(61)is_at_or_north_of Lat1,
      the_Lat_of_2D_pt(Two_D_pt,Lat1),
      Lat1 is_at_or_north_of lat_N(45)
      ;
      ( the_Long_of_2D_pt(Two_D_pt,Long1),
        long_W(15)is_at_or_west_of Long1,
        the_Long_of_2D_pt(Two_D_pt,Long1),
        Long1 is_at_or_west_of long_W(10),
        the_Lat_of_2D_pt(Two_D_pt,Lat1),
        lat_N(61)is_at_or_north_of Lat1,
        the_Lat_of_2D_pt(Two_D_pt,Lat1),
        Lat1 is_at_or_north_of lat_N(54)
        ;
        the_Long_of_2D_pt(Two_D_pt,Long1),
        long_W(15)is_at_or_west_of Long1,
        the_Long_of_2D_pt(Two_D_pt,Long1),
        Long1 is_at_or_west_of long_W(8),
        the_Lat_of_2D_pt(Two_D_pt,Lat1),
        lat_N(49)is_at_or_north_of Lat1,
        the_Lat_of_2D_pt(Two_D_pt,Lat1),
        Lat1 is_at_or_north_of lat_N(45)
      )
    ),
    !.

is_within_lateral_and_longitudinal_boundaries_of_airspace(Two_D_pt,oceanic):-
    ( the_Long_of_2D_pt(Two_D_pt,Long1),
      long_W(50)is_at_or_west_of Long1,
      the_Long_of_2D_pt(Two_D_pt,Long1),
      Long1 is_at_or_west_of long_W(15),
      the_Lat_of_2D_pt(Two_D_pt,Lat1),
      lat_N(61)is_at_or_north_of Lat1,
      the_Lat_of_2D_pt(Two_D_pt,Lat1),
      Lat1 is_at_or_north_of lat_N(45)
      ;
      ( the_Long_of_2D_pt(Two_D_pt,Long1),
        long_W(15)is_at_or_west_of Long1,
        the_Long_of_2D_pt(Two_D_pt,Long1),
        Long1 is_at_or_west_of long_W(10),
        the_Lat_of_2D_pt(Two_D_pt,Lat1),
        lat_N(61)is_at_or_north_of Lat1,
        the_Lat_of_2D_pt(Two_D_pt,Lat1),
        Lat1 is_at_or_north_of lat_N(54)
        ;
        the_Long_of_2D_pt(Two_D_pt,Long1),
        long_W(15)is_at_or_west_of Long1,
        the_Long_of_2D_pt(Two_D_pt,Long1),
        Long1 is_at_or_west_of long_W(8),
        the_Lat_of_2D_pt(Two_D_pt,Lat1),
        lat_N(49)is_at_or_north_of Lat1,
        the_Lat_of_2D_pt(Two_D_pt,Lat1),
        Lat1 is_at_or_north_of lat_N(45)
      )
    ),
    !.

is_within_lateral_and_longitudinal_boundaries_of_airspace(Two_D_pt,mnps):-
    is_within_lateral_and_longitudinal_boundaries_of_airspace(Two_D_pt,oceanic),
    !.

is_the_max_flight_level_for(fl(1000),shanwick).

is_the_min_flight_level_for(fl(55),shanwick).

are_vertices_for(twoD_pt(lat_N(61),long_W(30)),twoD_pt(lat_N(61),long_W(10)),shanwick).

are_vertices_for(twoD_pt(lat_N(61),long_W(10)),twoD_pt(lat_N(54,34),long_W(10)),shanwick).

are_vertices_for(twoD_pt(lat_N(54,34),long_W(10)),twoD_pt(lat_N(54),long_W(15)),shanwick).

are_vertices_for(twoD_pt(lat_N(54),long_W(15)),twoD_pt(lat_N(49),long_W(15)),shanwick).

are_vertices_for(twoD_pt(lat_N(49),long_W(15)),twoD_pt(lat_N(48,30),long_W(8)),shanwick).

are_vertices_for(twoD_pt(lat_N(48,30),long_W(8)),twoD_pt(lat_N(45),long_W(8)),shanwick).

are_vertices_for(twoD_pt(lat_N(45),long_W(8)),twoD_pt(lat_N(45),long_W(30)),shanwick).

are_vertices_for(twoD_pt(lat_N(45),long_W(30)),twoD_pt(lat_N(61),long_W(30)),shanwick).

is_the_max_flight_level_for(fl(1000),oceanic).

is_the_min_flight_level_for(fl(55),oceanic).

are_vertices_for(twoD_pt(lat_N(61),long_W(30)),twoD_pt(lat_N(61),long_W(10)),oceanic).

are_vertices_for(twoD_pt(lat_N(61),long_W(10)),twoD_pt(lat_N(54,34),long_W(10)),oceanic).

are_vertices_for(twoD_pt(lat_N(54,34),long_W(10)),twoD_pt(lat_N(54),long_W(15)),oceanic).

are_vertices_for(twoD_pt(lat_N(54),long_W(15)),twoD_pt(lat_N(49),long_W(15)),oceanic).

are_vertices_for(twoD_pt(lat_N(49),long_W(15)),twoD_pt(lat_N(48,30),long_W(8)),oceanic).

are_vertices_for(twoD_pt(lat_N(48,30),long_W(8)),twoD_pt(lat_N(45),long_W(8)),oceanic).

are_vertices_for(twoD_pt(lat_N(45),long_W(8)),twoD_pt(lat_N(45),long_W(30)),oceanic).

are_vertices_for(twoD_pt(lat_N(45),long_W(30)),twoD_pt(lat_N(61),long_W(30)),oceanic).

is_the_max_flight_level_for(fl(400),mnps).

is_the_min_flight_level_for(fl(275),mnps).

is_a_turbo_type(ac6t).

is_a_piston_type(ac56).

is_a_turbo_type(atla).

is_a_jet_type(b52).

is_a_jet_type(b707).

is_a_jet_type(b727).

is_a_jet_type(b737).

is_a_jet_type(b73s).

is_a_jet_type(b747).

is_a_jet_type(b74f).

is_a_jet_type(b74s).

is_a_jet_type(b757).

is_a_jet_type(b767).

is_a_jet_type(ba11).

is_a_turbo_type(ba31).

is_a_turbo_type(ba32).

is_a_jet_type(ba46).

is_a_turbo_type(be20).

is_a_turbo_type(be30).

is_a_turbo_type(c2).

is_a_jet_type(c5a).

is_a_jet_type(c9).

is_a_turbo_type(c12).

is_a_jet_type(ca20a).

is_a_jet_type(c21).

is_a_turbo_type(c130).

is_a_jet_type(c135).

is_a_jet_type(c137).

is_a_jet_type(c140).

is_a_jet_type(c141).

is_a_piston_type(c404).

is_a_turbo_type(c441).

is_a_jet_type(c500).

is_a_jet_type(conc).

is_a_jet_type(cl60).

is_a_jet_type(cl65).

is_a_jet_type(da20).

is_a_jet_type(da50).

is_a_jet_type(da90).

is_a_jet_type(dc8).

is_a_jet_type(dc8s).

is_a_jet_type(dc86).

is_a_jet_type(dc9).

is_a_jet_type(dc10).

is_a_turbo_type(dh4).

is_a_turbo_type(dh5).

is_a_turbo_type(dh7).

is_a_turbo_type(dh8).

is_a_jet_type(ea30).

is_a_jet_type(ea31).

is_a_jet_type(ea32).

is_a_jet_type(ea34).

is_a_jet_type(e3a).

is_a_jet_type(e4a).

is_a_jet_type(e6).

is_a_jet_type(f111).

is_a_jet_type(f4).

is_a_jet_type(f14).

is_a_jet_type(f15).

is_a_jet_type(f16).

is_a_jet_type(f18).

is_a_jet_type(fk28).

is_a_jet_type(g2).

is_a_jet_type(g3).

is_a_jet_type(g4).

is_a_jet_type(har).

is_a_jet_type(hs25).

is_a_turbo_type(hs74).

is_a_jet_type(il62).

is_a_jet_type(il86).

is_a_jet_type(kc10).

is_a_jet_type(kc35).

is_a_jet_type(l101).

is_a_jet_type(l329).

is_a_jet_type(lr35).

is_a_jet_type(lr55).

is_a_jet_type(md11).

is_a_jet_type(md80).

is_a_jet_type(mrc).

is_a_jet_type(n265).

is_a_turbo_type(nd16).

is_a_jet_type(nim).

is_a_turbo_type(p140).

is_a_turbo_type(p3).

is_a_turbo_type(p3c).

is_a_piston_type(pa30).

is_a_piston_type(pa31).

is_a_piston_type(pa34).

is_a_turbo_type(pa42).

is_a_piston_type(pa60).

is_a_jet_type(tu34).

is_a_jet_type(t39).

is_a_jet_type(vc10).

is_a_jet_type(vctr).

is_a_jet_type(u2).

is_a_jet_type(typ1).

is_a_jet_type(typ2).

is_a_jet_type(typ3).

is_a_turbo_type(typ4).

is_a_piston_type(typ5).

is_a_jet_type(typ6).

is_a_turbo_type(typ7).

is_a_prox_airfield_pt(twoD_pt(lat_N(67,1),long_W(50,41))).

is_a_prox_airfield_pt(twoD_pt(lat_N(63,59),long_W(22,36))).

is_a_prox_airfield_pt(twoD_pt(lat_N(63,59),long_W(22,36))).

is_a_prox_airfield_pt(twoD_pt(lat_N(63,45),long_W(68,33))).

is_a_prox_airfield_pt(twoD_pt(lat_N(44,53),long_W(63,31))).

is_a_prox_airfield_pt(twoD_pt(lat_N(48,33),long_W(58,33))).

is_a_prox_airfield_pt(twoD_pt(lat_N(45,41),long_W(74,2))).

is_a_prox_airfield_pt(twoD_pt(lat_N(45,19),long_W(75,40))).

is_a_prox_airfield_pt(twoD_pt(lat_N(46,48),long_W(71,24))).

is_a_prox_airfield_pt(twoD_pt(lat_N(48,56),long_W(54,34))).

is_a_prox_airfield_pt(twoD_pt(lat_N(53,19),long_W(60,26))).

is_a_prox_airfield_pt(twoD_pt(lat_N(47,37),long_W(52,45))).

is_a_prox_airfield_pt(twoD_pt(lat_N(43,41),long_W(79,38))).

is_a_prox_airfield_pt(twoD_pt(lat_N(45,28),long_W(73,45))).

is_a_prox_airfield_pt(twoD_pt(lat_N(50,54),long_E(4,32))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,12),long_E(2,52))).

is_a_prox_airfield_pt(twoD_pt(lat_N(49,57),long_E(6,34))).

is_a_prox_airfield_pt(twoD_pt(lat_N(49,59),long_E(6,42))).

is_a_prox_airfield_pt(twoD_pt(lat_N(50,2),long_E(8,34))).

is_a_prox_airfield_pt(twoD_pt(lat_N(49,57),long_E(7,16))).

is_a_prox_airfield_pt(twoD_pt(lat_N(48,22),long_E(7,50))).

is_a_prox_airfield_pt(twoD_pt(lat_N(49,26),long_E(7,36))).

is_a_prox_airfield_pt(twoD_pt(lat_N(53,46),long_E(8,40))).

is_a_prox_airfield_pt(twoD_pt(lat_N(50,2),long_E(8,34))).

is_a_prox_airfield_pt(twoD_pt(lat_N(53,38),long_E(9,59))).

is_a_prox_airfield_pt(twoD_pt(lat_N(50,52),long_E(7,9))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,17),long_E(6,45))).

is_a_prox_airfield_pt(twoD_pt(lat_N(48,41),long_E(9,12))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,55),long_E(8,18))).

is_a_prox_airfield_pt(twoD_pt(lat_N(52,28),long_E(9,41))).

is_a_prox_airfield_pt(twoD_pt(lat_N(54,34),long_W(6,13))).

is_a_prox_airfield_pt(twoD_pt(lat_N(52,27),long_W(1,45))).

is_a_prox_airfield_pt(twoD_pt(lat_N(53,21),long_W(2,16))).

is_a_prox_airfield_pt(twoD_pt(lat_N(50,26),long_W(5,0))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,30),long_W(1,59))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,9),long_W(1,45))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,24),long_W(3,21))).

is_a_prox_airfield_pt(twoD_pt(lat_N(53,21),long_W(2,53))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,53),long_W(0,22))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,9),long_W(0,11))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,28),long_W(0,27))).

is_a_prox_airfield_pt(twoD_pt(lat_N(53,52),long_W(1,39))).

is_a_prox_airfield_pt(twoD_pt(lat_N(55,2),long_W(1,41))).

is_a_prox_airfield_pt(twoD_pt(lat_N(52,50),long_W(1,19))).

is_a_prox_airfield_pt(twoD_pt(lat_N(57,12),long_W(2,12))).

is_a_prox_airfield_pt(twoD_pt(lat_N(55,57),long_W(4,26))).

is_a_prox_airfield_pt(twoD_pt(lat_N(55,57),long_W(3,22))).

is_a_prox_airfield_pt(twoD_pt(lat_N(55,30),long_W(4,35))).

is_a_prox_airfield_pt(twoD_pt(lat_N(58,13),long_W(6,20))).

is_a_prox_airfield_pt(twoD_pt(lat_N(55,29),long_W(5,41))).

is_a_prox_airfield_pt(twoD_pt(lat_N(57,36),long_W(3,33))).

is_a_prox_airfield_pt(twoD_pt(lat_N(56,22),long_W(2,51))).

is_a_prox_airfield_pt(twoD_pt(lat_N(57,43),long_W(3,20))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,53),long_W(0,14))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,56),long_W(1,15))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,17),long_W(0,46))).

is_a_prox_airfield_pt(twoD_pt(lat_N(52,25),long_E(0,34))).

is_a_prox_airfield_pt(twoD_pt(lat_N(52,22),long_E(0,29))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,41),long_W(1,47))).

is_a_prox_airfield_pt(twoD_pt(lat_N(52,5),long_E(1,24))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,45),long_W(1,35))).

is_a_prox_airfield_pt(twoD_pt(lat_N(52,22),long_W(0,13))).

is_a_prox_airfield_pt(twoD_pt(lat_N(53,18),long_W(0,33))).

is_a_prox_airfield_pt(twoD_pt(lat_N(52,18),long_E(4,46))).

is_a_prox_airfield_pt(twoD_pt(lat_N(50,55),long_E(5,46))).

is_a_prox_airfield_pt(twoD_pt(lat_N(52,10),long_E(4,25))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,58),long_E(4,27))).

is_a_prox_airfield_pt(twoD_pt(lat_N(51,53),long_W(8,30))).

is_a_prox_airfield_pt(twoD_pt(lat_N(53,26),long_W(6,15))).

is_a_prox_airfield_pt(twoD_pt(lat_N(53,55),long_W(8,49))).

is_a_prox_airfield_pt(twoD_pt(lat_N(52,47),long_W(8,55))).

is_a_prox_airfield_pt(twoD_pt(lat_N(49,37),long_E(6,12))).

is_a_prox_airfield_pt(twoD_pt(lat_N(60,17),long_E(5,13))).

is_a_prox_airfield_pt(twoD_pt(lat_N(58,53),long_E(5,38))).

is_a_prox_airfield_pt(twoD_pt(lat_N(38,49),long_W(76,52))).

is_a_prox_airfield_pt(twoD_pt(lat_N(39,11),long_W(76,40))).

is_a_prox_airfield_pt(twoD_pt(lat_N(41,56),long_W(72,41))).

is_a_prox_airfield_pt(twoD_pt(lat_N(44,48),long_W(68,50))).

is_a_prox_airfield_pt(twoD_pt(lat_N(41,23),long_W(71,8))).

is_a_prox_airfield_pt(twoD_pt(lat_N(42,56),long_W(78,44))).

is_a_prox_airfield_pt(twoD_pt(lat_N(39,11),long_W(76,40))).

is_a_prox_airfield_pt(twoD_pt(lat_N(39,8),long_W(75,28))).

is_a_prox_airfield_pt(twoD_pt(lat_N(40,42),long_W(74,10))).

is_a_prox_airfield_pt(twoD_pt(lat_N(38,57),long_W(77,24))).

is_a_prox_airfield_pt(twoD_pt(lat_N(43,6),long_W(78,57))).

is_a_prox_airfield_pt(twoD_pt(lat_N(40,38),long_W(73,46))).

is_a_prox_airfield_pt(twoD_pt(lat_N(36,56),long_W(76,17))).

is_a_prox_airfield_pt(twoD_pt(lat_N(43,54),long_W(69,57))).

is_a_prox_airfield_pt(twoD_pt(lat_N(44,39),long_W(73,28))).

is_a_prox_airfield_pt(twoD_pt(lat_N(39,52),long_W(75,15))).

is_a_prox_airfield_pt(twoD_pt(lat_N(43,5),long_W(70,49))).

is_a_prox_airfield_pt(twoD_pt(lat_N(41,43),long_W(71,25))).

is_a_prox_airfield_pt(twoD_pt(lat_N(40,51),long_W(74,4))).

is_a_prox_airfield_pt(twoD_pt(lat_N(40,1),long_W(74,36))).

is_a_prox_airfield_pt(twoD_pt(lat_N(41,18),long_E(2,5))).

is_a_prox_airfield_pt(twoD_pt(lat_N(38,52),long_E(1,21))).

is_a_prox_airfield_pt(twoD_pt(lat_N(40,28),long_W(3,33))).

is_a_prox_airfield_pt(twoD_pt(lat_N(36,41),long_W(4,30))).

is_a_prox_airfield_pt(twoD_pt(lat_N(39,33),long_E(2,44))).

is_a_prox_airfield_pt(twoD_pt(lat_N(42,54),long_W(8,25))).

is_a_prox_airfield_pt(twoD_pt(lat_N(40,29),long_W(3,27))).

is_a_prox_airfield_pt(twoD_pt(lat_N(37,25),long_W(5,54))).

is_a_prox_airfield_pt(twoD_pt(lat_N(44,52),long_W(0,42))).

is_a_prox_airfield_pt(twoD_pt(lat_N(45,44),long_E(5,5))).

is_a_prox_airfield_pt(twoD_pt(lat_N(43,40),long_E(7,13))).

is_a_prox_airfield_pt(twoD_pt(lat_N(48,58),long_E(2,27))).

is_a_prox_airfield_pt(twoD_pt(lat_N(48,43),long_E(2,23))).

is_a_prox_airfield_pt(twoD_pt(lat_N(49,1),long_E(2,33))).

is_a_prox_airfield_pt(twoD_pt(lat_N(48,26),long_W(4,25))).

is_a_prox_airfield_pt(twoD_pt(lat_N(47,46),long_W(3,26))).

is_a_prox_airfield_pt(twoD_pt(lat_N(47,14),long_W(1,36))).

is_a_prox_airfield_pt(twoD_pt(lat_N(50,33),long_E(3,5))).

is_a_prox_airfield_pt(twoD_pt(lat_N(47,35),long_E(7,32))).

is_a_prox_airfield_pt(twoD_pt(lat_N(45,38),long_E(8,44))).

is_a_prox_airfield_pt(twoD_pt(lat_N(45,12),long_E(7,39))).

is_a_prox_airfield_pt(twoD_pt(lat_N(44,24),long_E(8,51))).

is_a_prox_airfield_pt(twoD_pt(lat_N(36,58),long_W(25,10))).

is_a_prox_airfield_pt(twoD_pt(lat_N(37,0),long_W(7,58))).

is_a_prox_airfield_pt(twoD_pt(lat_N(38,46),long_W(27,6))).

is_a_prox_airfield_pt(twoD_pt(lat_N(38,46),long_W(9,8))).

is_a_prox_airfield_pt(twoD_pt(lat_N(46,16),long_E(6,7))).

is_a_prox_airfield_pt(twoD_pt(lat_N(47,27),long_E(8,33))).

is_a_prox_airfield_pt(twoD_pt(lat_N(36,9),long_W(5,21))).


