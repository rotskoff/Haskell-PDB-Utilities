-- Coordinate Table for ideal desolvation, Cambridge Cluster
-- Used to compare desolvation geometry in situ
-- WARNING! These atoms are partially initialized. Do not attempt to use them for anything other than their coordinates!
-- Note, there will be a warning issued for every atom. 
module IdealDesolvationCalcium where

import PDBparse
import PDButil
import qualified Data.ByteString.Char8 as B


ideals = [ideal1,ideal2,ideal3,ideal4,ideal5,ideal6,ideal7,ideal8,ideal9,ideal10,ideal11,ideal12,ideal13,ideal14,ideal15,ideal16,ideal17,ideal18,ideal19,ideal20]


ideal1 = [Atom {name=B.pack "O",coords=[0.000000,-0.000000,-1.631752]},
          Atom {name=B.pack "CA",coords=[0.000000,0.000000,0.763582]}]

ideal2 = [Atom {name=B.pack "O",coords=[-0.000000,2.398766,0.000000]},
          Atom {name=B.pack "O",coords=[-0.000000,-2.398766,0.000000]},
          Atom {name=B.pack "CA",coords=[0.000000,-0.000000,-0.000000]}]

ideal3 = [Atom {name=B.pack "O",coords=[-1.202171,-2.082222,-0.000000]},
          Atom {name=B.pack "O",coords=[-1.202171,2.082222,-0.000000]},
          Atom {name=B.pack "O",coords=[2.404343,0.000000,0.000000]},
          Atom {name=B.pack "CA",coords=[-0.000000,0.000000,0.000000]}]

ideal4 = [Atom {name=B.pack "O",coords=[0.210753,2.279142,0.758268]},
          Atom {name=B.pack "O",coords=[1.171334,-0.329390,-2.081671]},
          Atom {name=B.pack "O",coords=[-2.307493,-0.561434,-0.417308]},
          Atom {name=B.pack "O",coords=[0.925406,-1.388318,1.740711]},
          Atom {name=B.pack "CA",coords=[-0.000000,-0.000000,-0.000000]}]

ideal5 = [Atom {name=B.pack "O",coords=[-0.000000,0.000000,-2.343202]},
          Atom {name=B.pack "O",coords=[0.000001,2.363424,0.599749]},
          Atom {name=B.pack "O",coords=[-0.000000,-2.363424,0.599748]},
          Atom {name=B.pack "O",coords=[-2.392562,0.000000,0.494393]},
          Atom {name=B.pack "O",coords=[2.392562,-0.000000,0.494393]},
          Atom {name=B.pack "CA",coords=[0.000000,-0.000000,0.076814]}]

ideal6 = [Atom {name=B.pack "O",coords=[0.000000,0.000000,2.434325]},
          Atom {name=B.pack "O",coords=[0.000000,-2.434325,0.000000]},
          Atom {name=B.pack "O",coords=[0.000000,2.434325,0.000000]},
          Atom {name=B.pack "O",coords=[-2.434325,0.000000,0.000000]},
          Atom {name=B.pack "O",coords=[-0.000000,0.000000,-2.434325]},
          Atom {name=B.pack "O",coords=[2.434325,-0.000000,-0.000000]},
          Atom {name=B.pack "CA",coords=[0.000000,-0.000000,-0.000000]}]

ideal7 = [Atom {name=B.pack "O",coords=[2.428240,-0.000002,-0.000000]},
          Atom {name=B.pack "O",coords=[0.439720,-1.739543,-1.687553]},
          Atom {name=B.pack "O",coords=[0.439723,1.739542,1.687553]},
          Atom {name=B.pack "O",coords=[0.429008,1.461703,-1.917106]},
          Atom {name=B.pack "O",coords=[0.429005,-1.461704,1.917106]},
          Atom {name=B.pack "O",coords=[-2.033630,1.492403,-0.098336]},
          Atom {name=B.pack "O",coords=[-2.033633,-1.492400,0.098336]},
          Atom {name=B.pack "CA",coords=[-0.050743,0.000000,0.000000]}]

ideal8 = [Atom {name=B.pack "O",coords=[1.216428,1.736157,1.340249]},
          Atom {name=B.pack "O",coords=[-1.216428,-1.736157,1.340249]},
          Atom {name=B.pack "O",coords=[-0.367504,2.087793,-1.340249]},
          Atom {name=B.pack "O",coords=[-2.087793,-0.367504,-1.340249]},
          Atom {name=B.pack "O",coords=[-1.736157,1.216428,1.340249]},
          Atom {name=B.pack "O",coords=[2.087793,0.367504,-1.340249]},
          Atom {name=B.pack "O",coords=[0.367504,-2.087793,-1.340249]},
          Atom {name=B.pack "O",coords=[1.736157,-1.216428,1.340249]},
          Atom {name=B.pack "CA",coords=[-0.000000,-0.000000,-0.000000]}]

ideal9 = [Atom {name=B.pack "O",coords=[0.153730,-0.621529,2.423261]},
          Atom {name=B.pack "O",coords=[-3.975711,1.367272,0.039789]},
          Atom {name=B.pack "O",coords=[-0.153744,-0.621549,-2.423255]},
          Atom {name=B.pack "O",coords=[2.345545,-0.822670,-0.499494]},
          Atom {name=B.pack "O",coords=[-1.331474,1.700937,-0.714334]},
          Atom {name=B.pack "O",coords=[-2.345546,-0.822670,0.499483]},
          Atom {name=B.pack "O",coords=[0.000004,-2.738242,0.000013]},
          Atom {name=B.pack "O",coords=[1.331480,1.700936,0.714332]},
          Atom {name=B.pack "O",coords=[3.975714,1.367268,-0.039798]},
          Atom {name=B.pack "CA",coords=[0.000001,-0.254984,0.000001]}]

ideal10 = [ Atom {name=B.pack "O",coords=[-1.743604,-3.648689,0.537848]},
            Atom {name=B.pack "O",coords=[2.075369,-1.246678,0.389560]},
            Atom {name=B.pack "O",coords=[2.009282,1.694406,-0.022360]},
            Atom {name=B.pack "O",coords=[-2.161208,0.623259,-0.978362]},
            Atom {name=B.pack "O",coords=[-0.909658,-1.224817,1.610204]},
            Atom {name=B.pack "O",coords=[4.310047,0.352390,0.736529]},
            Atom {name=B.pack "O",coords=[-3.226837,2.653826,0.575240]},
            Atom {name=B.pack "O",coords=[-0.676739,1.884374,1.336356]},
            Atom {name=B.pack "O",coords=[-0.453017,-2.019927,-1.304314]},
            Atom {name=B.pack "O",coords=[0.567554,0.723445,-2.522112]},
            Atom {name=B.pack "CA",coords=[0.108505,0.105032,-0.181930]}]

ideal11 = [ Atom {name=B.pack "O",coords=[2.677439,-3.366544,0.159991]},
            Atom {name=B.pack "O",coords=[-0.841975,-0.517622,2.325449]},
            Atom {name=B.pack "O",coords=[0.711090,2.005528,1.250822]},
            Atom {name=B.pack "O",coords=[2.677442,3.366542,-0.159989]},
            Atom {name=B.pack "O",coords=[0.711091,-2.005527,-1.250823]},
            Atom {name=B.pack "O",coords=[-4.445926,-0.000000,-0.000001]},
            Atom {name=B.pack "O",coords=[1.865055,-0.946112,1.255960]},
            Atom {name=B.pack "O",coords=[-2.099771,1.470353,0.292143]},
            Atom {name=B.pack "O",coords=[-0.841969,0.517627,-2.325450]},
            Atom {name=B.pack "O",coords=[-2.099770,-1.470353,-0.292146]},
            Atom {name=B.pack "O",coords=[1.865060,0.946108,-1.255956]},
            Atom {name=B.pack "CA",coords=[-0.093531,0.000000,-0.000000]}]

ideal12 = [ Atom {name=B.pack "CA",coords=[1.520850,-1.715211,-1.549489]},
            Atom {name=B.pack "O",coords=[0.676861,-4.961225,1.187465]},
            Atom {name=B.pack "O",coords=[3.036414,0.238904,-1.914164]},
            Atom {name=B.pack "O",coords=[2.250378,-0.956569,0.717831]},
            Atom {name=B.pack "O",coords=[2.364838,1.530804,-4.286443]},
            Atom {name=B.pack "O",coords=[0.509636,-3.557232,-2.903332]},
            Atom {name=B.pack "O",coords=[0.644218,1.148386,1.576399]},
            Atom {name=B.pack "O",coords=[2.397482,-4.578810,-4.675375]},
            Atom {name=B.pack "O",coords=[-0.287295,-2.713581,-0.141528]},
            Atom {name=B.pack "O",coords=[0.781754,-0.683001,-3.702804]},
            Atom {name=B.pack "O",coords=[3.338564,-2.507692,-3.071454]},
            Atom {name=B.pack "O",coords=[2.552528,-3.703165,-0.439459]},
            Atom {name=B.pack "O",coords=[-0.015176,0.160649,-0.941000]}]

ideal13 = [ Atom {name=B.pack "CA",coords=[-0.732742,3.208439,-0.246717]},
            Atom {name=B.pack "O",coords=[1.499095,1.521134,-3.627127]},
            Atom {name=B.pack "O",coords=[0.982080,4.123149,1.314735]},
            Atom {name=B.pack "O",coords=[-2.763506,5.096844,3.154047]},
            Atom {name=B.pack "O",coords=[1.464337,2.183887,-0.912749]},
            Atom {name=B.pack "O",coords=[-3.149236,2.977222,-0.801648]},
            Atom {name=B.pack "O",coords=[-3.552007,1.992678,-3.379597]},
            Atom {name=B.pack "O",coords=[-1.646688,2.787238,2.035726]},
            Atom {name=B.pack "O",coords=[-1.830643,5.332326,0.530573]},
            Atom {name=B.pack "O",coords=[0.325544,5.067235,-1.527478]},
            Atom {name=B.pack "O",coords=[-2.242988,0.094073,2.447935]},
            Atom {name=B.pack "O",coords=[-1.006000,0.749521,0.035961]},
            Atom {name=B.pack "O",coords=[-0.917194,2.531086,-2.640338]},
            Atom {name=B.pack "O",coords=[2.295425,6.237041,0.059791]}]

ideal14 = [ Atom {name=B.pack "CA",coords=[2.326721,-0.826193,3.599282]},
            Atom {name=B.pack "O",coords=[2.302128,-1.492667,8.429307]},
            Atom {name=B.pack "O",coords=[0.720802,-3.765716,0.775068]},
            Atom {name=B.pack "O",coords=[2.723937,0.029381,-1.184456]},
            Atom {name=B.pack "O",coords=[1.848783,-1.239976,1.167187]},
            Atom {name=B.pack "O",coords=[2.250935,1.487528,2.639877]},
            Atom {name=B.pack "O",coords=[3.940970,-2.359171,4.749794]},
            Atom {name=B.pack "O",coords=[-0.089133,-0.255201,3.386681]},
            Atom {name=B.pack "O",coords=[4.152983,1.524190,0.651906]},
            Atom {name=B.pack "O",coords=[4.668537,-1.061353,7.064415]},
            Atom {name=B.pack "O",coords=[1.249177,-1.201787,5.838155]},
            Atom {name=B.pack "O",coords=[-1.485401,-0.636221,5.750469]},
            Atom {name=B.pack "O",coords=[3.560522,0.629299,5.193090]},
            Atom {name=B.pack "O",coords=[1.360721,-3.113412,3.391790]},
            Atom {name=B.pack "O",coords=[4.502932,-0.551166,2.429064]}]

ideal15 = [ Atom {name=B.pack "CA",coords=[-0.004747,-2.610336,1.521578]},
            Atom {name=B.pack "O",coords=[-0.993994,-0.826406,0.077517]},
            Atom {name=B.pack "O",coords=[0.077417,-6.710028,0.815082]},
            Atom {name=B.pack "O",coords=[1.878272,-1.758493,0.140112]},
            Atom {name=B.pack "O",coords=[0.594333,2.132564,1.720856]},
            Atom {name=B.pack "O",coords=[0.602690,-3.142616,3.877881]},
            Atom {name=B.pack "O",coords=[-0.582345,-3.596358,-0.695906]},
            Atom {name=B.pack "O",coords=[-0.609624,-6.162614,-1.809005]},
            Atom {name=B.pack "O",coords=[-3.014573,0.486590,1.408456]},
            Atom {name=B.pack "O",coords=[1.692054,-0.968197,5.210299]},
            Atom {name=B.pack "O",coords=[1.112481,0.788526,-0.642668]},
            Atom {name=B.pack "O",coords=[-2.031508,3.044433,1.913616]},
            Atom {name=B.pack "O",coords=[0.733765,-0.489105,2.624878]},
            Atom {name=B.pack "O",coords=[-2.069686,-1.808089,2.668727]},
            Atom {name=B.pack "O",coords=[-1.336665,-4.668469,2.013766]},
            Atom {name=B.pack "O",coords=[1.625529,-4.501197,1.369710]}]

ideal16 = [ Atom {name=B.pack "CA",coords=[0.447505,-0.508124,2.329206]},
            Atom {name=B.pack "O",coords=[1.045572,-0.883767,4.705243]},
            Atom {name=B.pack "O",coords=[-1.450314,-0.255235,0.715432]},
            Atom {name=B.pack "O",coords=[1.366747,-0.725195,0.026343]},
            Atom {name=B.pack "O",coords=[-4.358194,3.039102,1.571609]},
            Atom {name=B.pack "O",coords=[3.038547,3.017046,1.895193]},
            Atom {name=B.pack "O",coords=[2.817955,0.302433,2.553672]},
            Atom {name=B.pack "O",coords=[1.786811,-2.628367,2.334874]},
            Atom {name=B.pack "O",coords=[-3.535833,-1.092860,3.514923]},
            Atom {name=B.pack "O",coords=[5.038499,-0.724176,3.945763]},
            Atom {name=B.pack "O",coords=[0.476483,1.915692,1.800757]},
            Atom {name=B.pack "O",coords=[-1.344762,0.570693,3.683375]},
            Atom {name=B.pack "O",coords=[3.183224,-2.632191,4.709160]},
            Atom {name=B.pack "O",coords=[-0.587972,-0.531873,-1.931315]},
            Atom {name=B.pack "O",coords=[-1.851825,3.135883,2.791719]},
            Atom {name=B.pack "O",coords=[-1.147428,-2.335899,2.877159]},
            Atom {name=B.pack "O",coords=[-4.131296,0.291337,1.192671]}]

ideal17 = [ Atom {name=B.pack "CA",coords=[1.167366,1.159460,-0.485558]},
            Atom {name=B.pack "O",coords=[-3.638842,2.879311,-3.636248]},
            Atom {name=B.pack "O",coords=[3.063344,1.403023,1.133255]},
            Atom {name=B.pack "O",coords=[-0.887362,2.521067,-0.167253]},
            Atom {name=B.pack "O",coords=[-0.709002,-0.289429,-1.237700]},
            Atom {name=B.pack "O",coords=[1.803686,-1.653890,2.550183]},
            Atom {name=B.pack "O",coords=[-1.089557,3.990372,-3.545353]},
            Atom {name=B.pack "O",coords=[4.470661,-1.526656,-1.204836]},
            Atom {name=B.pack "O",coords=[6.155673,-1.853747,0.991692]},
            Atom {name=B.pack "O",coords=[3.210441,0.863009,-1.890028]},
            Atom {name=B.pack "O",coords=[1.939207,-1.180671,-0.162619]},
            Atom {name=B.pack "O",coords=[0.580178,1.904735,-2.810488]},
            Atom {name=B.pack "O",coords=[-0.362485,5.065950,-1.095362]},
            Atom {name=B.pack "O",coords=[-3.012956,1.287561,-1.435485]},
            Atom {name=B.pack "O",coords=[1.878608,3.531355,-0.506639]},
            Atom {name=B.pack "O",coords=[4.341632,-0.544128,2.651622]},
            Atom {name=B.pack "O",coords=[3.825537,4.089718,1.410172]},
            Atom {name=B.pack "O",coords=[0.257322,0.492661,1.724415]}]

ideal18 = [ Atom {name=B.pack "CA",coords=[-1.140200,-0.811660,-0.686964]},
            Atom {name=B.pack "O",coords=[-0.386012,-2.389965,-2.445759]},
            Atom {name=B.pack "O",coords=[-3.483739,-0.343501,-1.351685]},
            Atom {name=B.pack "O",coords=[1.287748,-0.260310,-0.956429]},
            Atom {name=B.pack "O",coords=[0.094617,2.921796,-2.276055]},
            Atom {name=B.pack "O",coords=[-5.152484,-2.399602,-0.440376]},
            Atom {name=B.pack "O",coords=[0.383029,-0.962298,-4.728977]},
            Atom {name=B.pack "O",coords=[-2.308172,-0.482931,1.503768]},
            Atom {name=B.pack "O",coords=[-2.439461,-2.925423,-0.363467]},
            Atom {name=B.pack "O",coords=[-3.513864,-2.341688,3.182539]},
            Atom {name=B.pack "O",coords=[-0.978541,1.577005,-0.077060]},
            Atom {name=B.pack "O",coords=[2.522735,1.618910,-2.590528]},
            Atom {name=B.pack "O",coords=[0.145871,-2.189102,0.910695]},
            Atom {name=B.pack "O",coords=[2.802778,0.381592,-5.069743]},
            Atom {name=B.pack "O",coords=[-6.083223,-2.703434,2.170899]},
            Atom {name=B.pack "O",coords=[-1.630478,-4.025064,2.037433]},
            Atom {name=B.pack "O",coords=[-2.323527,2.223736,2.287369]},
            Atom {name=B.pack "O",coords=[-1.038103,0.465896,-2.835952]},
            Atom {name=B.pack "O",coords=[2.913416,-1.842191,0.715448]}]

ideal19 = [ Atom {name=B.pack "CA",coords=[-1.869842,0.625230,-1.764693]},
            Atom {name=B.pack "O",coords=[0.053316,1.932838,-5.378770]},
            Atom {name=B.pack "O",coords=[-4.891003,-1.954680,-0.039132]},
            Atom {name=B.pack "O",coords=[-1.643703,-1.788764,-2.368642]},
            Atom {name=B.pack "O",coords=[-3.933951,1.472283,-5.823235]},
            Atom {name=B.pack "O",coords=[-4.208501,-0.122467,-2.057349]},
            Atom {name=B.pack "O",coords=[-0.104965,4.879832,-2.017694]},
            Atom {name=B.pack "O",coords=[-1.590130,-4.078548,-0.814643]},
            Atom {name=B.pack "O",coords=[-0.971010,4.859305,0.611421]},
            Atom {name=B.pack "O",coords=[-3.185382,2.604422,-2.504202]},
            Atom {name=B.pack "O",coords=[-2.265276,4.890057,-3.741238]},
            Atom {name=B.pack "O",coords=[-5.561541,1.735931,-3.606295]},
            Atom {name=B.pack "O",coords=[-2.079986,3.521472,-6.138584]},
            Atom {name=B.pack "O",coords=[-0.411292,-2.251195,0.899833]},
            Atom {name=B.pack "O",coords=[-0.151006,2.192353,-2.643112]},
            Atom {name=B.pack "O",coords=[-4.239248,-4.664342,-0.190557]},
            Atom {name=B.pack "O",coords=[-1.699676,2.205374,0.144764]},
            Atom {name=B.pack "O",coords=[0.269277,-0.127177,-0.758636]},
            Atom {name=B.pack "O",coords=[-1.924457,0.402654,-4.252865]},
            Atom {name=B.pack "O",coords=[-2.523333,-0.573741,0.334577]}]


ideal20 = [ Atom {name=B.pack "CA",coords=[0.854599,1.371649,-0.683178]},
            Atom {name=B.pack "O",coords=[0.695283,-1.510181,2.898678]},
            Atom {name=B.pack "O",coords=[1.266909,0.934437,1.730887]},
            Atom {name=B.pack "O",coords=[0.147575,0.346594,-2.837766]},
            Atom {name=B.pack "O",coords=[2.270346,3.242241,0.142330]},
            Atom {name=B.pack "O",coords=[-1.826481,-0.449298,2.558952]},
            Atom {name=B.pack "O",coords=[-0.876587,-0.299570,-0.043122]},
            Atom {name=B.pack "O",coords=[4.624593,3.543788,-1.333942]},
            Atom {name=B.pack "O",coords=[-0.508053,5.286312,-0.266385]},
            Atom {name=B.pack "O",coords=[1.184174,-3.023802,0.631675]},
            Atom {name=B.pack "O",coords=[-1.031890,-2.072531,-3.610968]},
            Atom {name=B.pack "O",coords=[-1.809736,2.251687,3.064598]},
            Atom {name=B.pack "O",coords=[0.238761,3.327395,-2.124689]},
            Atom {name=B.pack "O",coords=[1.686392,1.588414,-4.773299]},
            Atom {name=B.pack "O",coords=[-0.877741,2.681191,0.511135]},
            Atom {name=B.pack "O",coords=[0.143289,3.886366,-4.866022]},
            Atom {name=B.pack "O",coords=[1.296286,5.152394,1.850625]},
            Atom {name=B.pack "O",coords=[2.118120,-0.764977,-0.630279]},
            Atom {name=B.pack "O",coords=[2.817065,1.610286,-2.231980]},
            Atom {name=B.pack "O",coords=[0.793121,3.011014,3.525660]},
            Atom {name=B.pack "O",coords=[-1.076277,-2.887926,-0.968442]}]




