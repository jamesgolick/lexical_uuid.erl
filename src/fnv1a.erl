-module(fnv1a).

-author("Cliff Moon").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([hash/1]).

-define(OFFSET_BASIS, 14695981039346656037).
-define(FNV_PRIME, 1099511628211).

hash(Term) when is_binary(Term) ->
  fnv_int(?OFFSET_BASIS, 0, Term);

hash(Term) ->
  fnv_int(?OFFSET_BASIS, 0, term_to_binary(Term)).

fnv_int(Hash, ByteOffset, Bin) when erlang:byte_size(Bin) == ByteOffset ->
  Hash;

fnv_int(Hash, ByteOffset, Bin) ->
  <<_:ByteOffset/binary, Octet:8, _/binary>> = Bin,
  Xord = Hash bxor Octet,
  fnv_int((Xord * ?FNV_PRIME) rem (2 bsl 63), ByteOffset+1, Bin).

-ifdef(TEST).

gen(Bytes, Times) ->
  gen(Bytes, Times, []).

gen(_, Times, Acc) when Times == 0 ->
  list_to_binary(lists:flatten(Acc));
gen(Bytes, Times, Acc) ->
  gen(Bytes, Times - 1, [Bytes | Acc]).

%% Converted from https://raw.github.com/jakedouglas/fnv-ruby/master/test/fnv_test.rb
fnv1a_test() ->
  Tests = [{<<"">>                                                                          , 16#cbf29ce484222325},
           {<<"a">>                                                                         , 16#af63dc4c8601ec8c},
           {<<"b">>                                                                         , 16#af63df4c8601f1a5},
           {<<"c">>                                                                         , 16#af63de4c8601eff2},
           {<<"d">>                                                                         , 16#af63d94c8601e773},
           {<<"e">>                                                                         , 16#af63d84c8601e5c0},
           {<<"f">>                                                                         , 16#af63db4c8601ead9},
           {<<"fo">>                                                                        , 16#08985907b541d342},
           {<<"foo">>                                                                       , 16#dcb27518fed9d577},
           {<<"foob">>                                                                      , 16#dd120e790c2512af},
           {<<"fooba">>                                                                     , 16#cac165afa2fef40a},
           {<<"foobar">>                                                                    , 16#85944171f73967e8},
           {<<"ch">>                                                                        , 16#08a25607b54a22ae},
           {<<"cho">>                                                                       , 16#f5faf0190cf90df3},
           {<<"chon">>                                                                      , 16#f27397910b3221c7},
           {<<"chong">>                                                                     , 16#2c8c2b76062f22e0},
           {<<"chongo">>                                                                    , 16#e150688c8217b8fd},
           {<<"chongo ">>                                                                   , 16#f35a83c10e4f1f87},
           {<<"chongo w">>                                                                  , 16#d1edd10b507344d0},
           {<<"chongo wa">>                                                                 , 16#2a5ee739b3ddb8c3},
           {<<"chongo was">>                                                                , 16#dcfb970ca1c0d310},
           {<<"chongo was ">>                                                               , 16#4054da76daa6da90},
           {<<"chongo was h">>                                                              , 16#f70a2ff589861368},
           {<<"chongo was he">>                                                             , 16#4c628b38aed25f17},
           {<<"chongo was her">>                                                            , 16#9dd1f6510f78189f},
           {<<"chongo was here">>                                                           , 16#a3de85bd491270ce},
           {<<"chongo was here!">>                                                          , 16#858e2fa32a55e61d},
           {<<"chongo was here!\n">>                                                        , 16#46810940eff5f915},
           {<<"cu">>                                                                        , 16#08a24307b54a0265},
           {<<"cur">>                                                                       , 16#f5b9fd190cc18d15},
           {<<"curd">>                                                                      , 16#4c968290ace35703},
           {<<"curds">>                                                                     , 16#07174bd5c64d9350},
           {<<"curds ">>                                                                    , 16#5a294c3ff5d18750},
           {<<"curds a">>                                                                   , 16#05b3c1aeb308b843},
           {<<"curds an">>                                                                  , 16#b92a48da37d0f477},
           {<<"curds and">>                                                                 , 16#73cdddccd80ebc49},
           {<<"curds and ">>                                                                , 16#d58c4c13210a266b},
           {<<"curds and w">>                                                               , 16#e78b6081243ec194},
           {<<"curds and wh">>                                                              , 16#b096f77096a39f34},
           {<<"curds and whe">>                                                             , 16#b425c54ff807b6a3},
           {<<"curds and whey">>                                                            , 16#23e520e2751bb46e},
           {<<"curds and whey\n">>                                                          , 16#1a0b44ccfe1385ec},
           {<<"hi">>                                                                        , 16#08ba5f07b55ec3da},
           {<<"hello">>                                                                     , 16#a430d84680aabd0b},
           {<<"\xff\x00\x00\x01">>                                                          , 16#6961196491cc682d},
           {<<"\x01\x00\x00\xff">>                                                          , 16#ad2bb1774799dfe9},
           {<<"\xff\x00\x00\x02">>                                                          , 16#6961166491cc6314},
           {<<"\x02\x00\x00\xff">>                                                          , 16#8d1bb3904a3b1236},
           {<<"\xff\x00\x00\x03">>                                                          , 16#6961176491cc64c7},
           {<<"\x03\x00\x00\xff">>                                                          , 16#ed205d87f40434c7},
           {<<"\xff\x00\x00\x04">>                                                          , 16#6961146491cc5fae},
           {<<"\x04\x00\x00\xff">>                                                          , 16#cd3baf5e44f8ad9c},
           {<<"\x40\x51\x4e\x44">>                                                          , 16#e3b36596127cd6d8},
           {<<"\x44\x4e\x51\x40">>                                                          , 16#f77f1072c8e8a646},
           {<<"\x40\x51\x4e\x4a">>                                                          , 16#e3b36396127cd372},
           {<<"\x4a\x4e\x51\x40">>                                                          , 16#6067dce9932ad458},
           {<<"\x40\x51\x4e\x54">>                                                          , 16#e3b37596127cf208},
           {<<"\x54\x4e\x51\x40">>                                                          , 16#4b7b10fa9fe83936},
           {<<"127.0.0.1">>                                                                 , 16#aabafe7104d914be},
           {<<"127.0.0.2">>                                                                 , 16#aabafd7104d9130b},
           {<<"127.0.0.3">>                                                                 , 16#aabafc7104d91158},
           {<<"64.81.78.68">>                                                               , 16#e729bac5d2a8d3a7},
           {<<"64.81.78.74">>                                                               , 16#e72630c5d2a5b352},
           {<<"64.81.78.84">>                                                               , 16#e73042c5d2ae266d},
           {<<"feedface">>                                                                  , 16#0a83c86fee952abc},
           {<<"feedfacedaffdeed">>                                                          , 16#3e66d3d56b8caca1},
           {<<"feedfacedeadbeef">>                                                          , 16#cac54572bb1a6fc8},
           {<<"line 1\nline 2\nline 3">>                                                    , 16#7829851fac17b143},
           {<<"chongo <Landon Curt Noll> /\\../\\">>                                        , 16#2c8f4c9af81bcf06},
           {<<"chongo (Landon Curt Noll) /\\../\\">>                                        , 16#3605a2ac253d2db1},
           {<<"http://antwrp.gsfc.nasa.gov/apod/astropix.html">>                            , 16#6be396289ce8a6da},
           {<<"http://en.wikipedia.org/wiki/Fowler_Noll_Vo_hash">>                          , 16#d9b957fb7fe794c5},
           {<<"http://epod.usra.edu/">>                                                     , 16#05be33da04560a93},
           {<<"http://exoplanet.eu/">>                                                      , 16#0957f1577ba9747c},
           {<<"http://hvo.wr.usgs.gov/cam3/">>                                              , 16#da2cc3acc24fba57},
           {<<"http://hvo.wr.usgs.gov/cams/HMcam/">>                                        , 16#74136f185b29e7f0},
           {<<"http://hvo.wr.usgs.gov/kilauea/update/deformation.html">>                    , 16#b2f2b4590edb93b2},
           {<<"http://hvo.wr.usgs.gov/kilauea/update/images.html">>                         , 16#b3608fce8b86ae04},
           {<<"http://hvo.wr.usgs.gov/kilauea/update/maps.html">>                           , 16#4a3a865079359063},
           {<<"http://hvo.wr.usgs.gov/volcanowatch/current_issue.html">>                    , 16#5b3a7ef496880a50},
           {<<"http://neo.jpl.nasa.gov/risk/">>                                             , 16#48fae3163854c23b},
           {<<"http://norvig.com/21-days.html">>                                            , 16#07aaa640476e0b9a},
           {<<"http://primes.utm.edu/curios/home.php">>                                     , 16#2f653656383a687d},
           {<<"http://slashdot.org/">>                                                      , 16#a1031f8e7599d79c},
           {<<"http://tux.wr.usgs.gov/Maps/155.25-19.5.html">>                              , 16#a31908178ff92477},
           {<<"http://volcano.wr.usgs.gov/kilaueastatus.php">>                              , 16#097edf3c14c3fb83},
           {<<"http://www.avo.alaska.edu/activity/Redoubt.php">>                            , 16#b51ca83feaa0971b},
           {<<"http://www.dilbert.com/fast/">>                                              , 16#dd3c0d96d784f2e9},
           {<<"http://www.fourmilab.ch/gravitation/orbits/">>                               , 16#86cd26a9ea767d78},
           {<<"http://www.fpoa.net/">>                                                      , 16#e6b215ff54a30c18},
           {<<"http://www.ioccc.org/index.html">>                                           , 16#ec5b06a1c5531093},
           {<<"http://www.isthe.com/cgi-bin/number.cgi">>                                   , 16#45665a929f9ec5e5},
           {<<"http://www.isthe.com/chongo/bio.html">>                                      , 16#8c7609b4a9f10907},
           {<<"http://www.isthe.com/chongo/index.html">>                                    , 16#89aac3a491f0d729},
           {<<"http://www.isthe.com/chongo/src/calc/lucas-calc">>                           , 16#32ce6b26e0f4a403},
           {<<"http://www.isthe.com/chongo/tech/astro/venus2004.html">>                     , 16#614ab44e02b53e01},
           {<<"http://www.isthe.com/chongo/tech/astro/vita.html">>                          , 16#fa6472eb6eef3290},
           {<<"http://www.isthe.com/chongo/tech/comp/c/expert.html">>                       , 16#9e5d75eb1948eb6a},
           {<<"http://www.isthe.com/chongo/tech/comp/calc/index.html">>                     , 16#b6d12ad4a8671852},
           {<<"http://www.isthe.com/chongo/tech/comp/fnv/index.html">>                      , 16#88826f56eba07af1},
           {<<"http://www.isthe.com/chongo/tech/math/number/howhigh.html">>                 , 16#44535bf2645bc0fd},
           {<<"http://www.isthe.com/chongo/tech/math/number/number.html">>                  , 16#169388ffc21e3728},
           {<<"http://www.isthe.com/chongo/tech/math/prime/mersenne.html">>                 , 16#f68aac9e396d8224},
           {<<"http://www.isthe.com/chongo/tech/math/prime/mersenne.html#largest">>         , 16#8e87d7e7472b3883},
           {<<"http://www.lavarnd.org/cgi-bin/corpspeak.cgi">>                              , 16#295c26caa8b423de},
           {<<"http://www.lavarnd.org/cgi-bin/haiku.cgi">>                                  , 16#322c814292e72176},
           {<<"http://www.lavarnd.org/cgi-bin/rand-none.cgi">>                              , 16#8a06550eb8af7268},
           {<<"http://www.lavarnd.org/cgi-bin/randdist.cgi">>                               , 16#ef86d60e661bcf71},
           {<<"http://www.lavarnd.org/index.html">>                                         , 16#9e5426c87f30ee54},
           {<<"http://www.lavarnd.org/what/nist-test.html">>                                , 16#f1ea8aa826fd047e},
           {<<"http://www.macosxhints.com/">>                                               , 16#0babaf9a642cb769},
           {<<"http://www.mellis.com/">>                                                    , 16#4b3341d4068d012e},
           {<<"http://www.nature.nps.gov/air/webcams/parks/havoso2alert/havoalert.cfm">>    , 16#d15605cbc30a335c},
           {<<"http://www.nature.nps.gov/air/webcams/parks/havoso2alert/timelines_24.cfm">> , 16#5b21060aed8412e5},
           {<<"http://www.paulnoll.com/">>                                                  , 16#45e2cda1ce6f4227},
           {<<"http://www.pepysdiary.com/">>                                                , 16#50ae3745033ad7d4},
           {<<"http://www.sciencenews.org/index/home/activity/view">>                       , 16#aa4588ced46bf414},
           {<<"http://www.skyandtelescope.com/">>                                           , 16#c1b0056c4a95467e},
           {<<"http://www.sput.nl/~rob/sirius.html">>                                       , 16#56576a71de8b4089},
           {<<"http://www.systemexperts.com/">>                                             , 16#bf20965fa6dc927e},
           {<<"http://www.tq-international.com/phpBB3/index.php">>                          , 16#569f8383c2040882},
           {<<"http://www.travelquesttours.com/index.htm">>                                 , 16#e1e772fba08feca0},
           {<<"http://www.wunderground.com/global/stations/89606.html">>                    , 16#4ced94af97138ac4},
           {gen("21701", 10)                                                                , 16#c4112ffb337a82fb},
           {gen("M21701", 10)                                                               , 16#d64a4fd41de38b7d},
           {gen("2^21701-1", 10)                                                            , 16#4cfc32329edebcbb},
           {gen("\x54\xc5", 10)                                                             , 16#0803564445050395},
           {gen("\xc5\x54", 10)                                                             , 16#aa1574ecf4642ffd},
           {gen("23209", 10)                                                                , 16#694bc4e54cc315f9},
           {gen("M23209", 10)                                                               , 16#a3d7cb273b011721},
           {gen("2^23209-1", 10)                                                            , 16#577c2f8b6115bfa5},
           {gen("\x5a\xa9", 10)                                                             , 16#b7ec8c1a769fb4c1},
           {gen("\xa9\x5a", 10)                                                             , 16#5d5cfce63359ab19},
           {gen("391581216093", 10)                                                         , 16#33b96c3cd65b5f71},
           {gen("391581*2^216093-1", 10)                                                    , 16#d845097780602bb9},
           {gen("\x05\xf9\x9d\x03\x4c\x81", 10)                                             , 16#84d47645d02da3d5},
           {gen("FEDCBA9876543210", 10)                                                     , 16#83544f33b58773a5},
           {gen("\xfe\xdc\xba\x98\x76\x54\x32\x10", 10)                                     , 16#9175cbb2160836c5},
           {gen("EFCDAB8967452301", 10)                                                     , 16#c71b3bc175e72bc5},
           {gen("\xef\xcd\xab\x89\x67\x45\x23\x01", 10)                                     , 16#636806ac222ec985},
           {gen("0123456789ABCDEF", 10)                                                     , 16#b6ef0e6950f52ed5},
           {gen("\x01\x23\x45\x67\x89\xab\xcd\xef", 10)                                     , 16#ead3d8a0f3dfdaa5},
           {gen("1032547698BADCFE", 10)                                                     , 16#922908fe9a861ba5},
           {gen("\x10\x32\x54\x76\x98\xba\xdc\xfe", 10)                                     , 16#6d4821de275fd5c5},
           {gen("\x00", 500)                                                                , 16#1fe3fce62bd816b5},
           {gen("\x07", 500)                                                                , 16#c23e9fccd6f70591},
           {gen("~", 500)                                                                   , 16#c1af12bdfe16b5b5},
           {gen("\x7f", 500)                                                                , 16#39e9f18f2f85e221}],
  lists:foreach(fun({String, Hash}) -> ?assertEqual(Hash, hash(String)) end, Tests),
  ok.

-endif.
