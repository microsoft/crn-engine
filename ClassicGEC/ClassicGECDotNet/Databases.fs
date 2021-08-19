module Microsoft.Research.GEC.Databases

let defaultParts = "i723017,pcr,codes(xylR;0.001)
i723024, pcr, codes(phzM;0.001)
e0040, pcr, codes(gfp;0.01)
c0099, pcr, codes(cviR;0.01)
i723025, pcr, codes(phzS;0.001)
i723028, pcr, codes(pca;0.001)
c0051, pcr, codes(cI;0.01)
c0040, pcr, codes(tetR;0.01)
c0080, pcr, codes(araC;0.01)
c0012, pcr, codes(lacI;0.01)
cunknown2, pcr, codes(unknown2;0.001)
c0061, pcr, codes(luxI;0.01)
c0062, pcr, codes(luxR;0.01)
c0079, pcr, codes(lasR;0.01)
c0078, pcr, codes(lasI;0.01)
cunknown3, pcr, codes(ccdB;0.005)
cunknown4, pcr, codes(ccdA;0.1)
i723020, prom, pos(toluene::xylR;0.001; 0.001; 1.0); con(0.0001)
r0051, prom, neg(cI; 1.0; 0.5; 0.00005); con(0.12)
r0040, prom, neg(tetR; 1.0; 0.5; 0.00005); con(0.09)
runknown1, prom, neg(unknown1; 1.0; 0.005; 0.001); con(0.04)
r0080, prom, neg(araC; 1.0; 0.000001; 0.0001); pos(araC::arabinose; 0.001; 0.001; 1.0); con(0.1)
r0011, prom, neg(lacI; 1.0; 0.5; 0.00005); con(0.1)
r0062, prom, pos(lasR::m3OC12HSL; 1.0; 0.8; 0.1); pos(luxR::m3OC6HSL; 1.0; 0.8; 0.1); con(0.01)
r0090, prom, pos(lasR::m3OC12HSL; 1.0; 0.8; 0.1); con(0.01)
r0099, prom, pos(cviR::m3OC6HSL; 1.0; 0.8; 0.1); con(0.01)
b0034, rbs, rate(0.1)
b0015, ter
cunknown5, pcr, codes(ccdA2; 10.0)
runknown5, prom, con(10.0)
j06504, pcr, codes(mCherry; 0.1)
prpr, device, components[pr; rbs34; eyfp; ter1; pr; rbs34; ecfp; ter1]
drPcat, device, components[pCat; rbs34; luxR; rbs34; lasR; ter1; pLas81; rbs34; eyfp; ter1; plux76; rbs34; ecfp; ter1]
drRS100S32, device, components[pTet; rbss100; luxR; ter1; pLac; rbs32; lasR; ter1; pLas81; rbs34; eyfp; ter1; plux76; rbs34; ecfp; ter1]
drR33S32, device, components[pTet; rbs33; luxR; ter1; pLac; rbs32; lasR; ter1; pLas81; rbs34; eyfp; ter1; plux76; rbs34; ecfp; ter1]
drR33S175, device, components[pTet; rbs33; luxR; ter1; pLac; rbsS175; lasR; ter1; pLas81; rbs34; eyfp; ter1; plux76; rbs34; ecfp; ter1]
relayP76LasI, device, components[pLux76; rbs900; lasI; l3s2p21]
relayP81LuxI, device, components[pLas81; rbs32; luxI; l3s2p21]
pBadYFP, device, components[pBad; rbs34; eyfp; l3s2p21]
lactonase, device, components[pBad; rbs34; aiia; l3s2p21]"

let defaultReactions = "toluene + xylR ->{1.0} toluene::xylR
phzM ~ pca ->{1.0} metPCA
phzS ~ metPCA ->{1.0} pyo
luxR + m3OC6HSL ->{0.5} luxR::m3OC6HSL
lasR + m3OC12HSL ->{0.5} lasR::m3OC12HSL
cviR + m3OC6HSL ->{0.5} cviR::m3OC6HSL
cviR + m3OC12HSL ->{0.5} cviR::m3OC12HSL
luxI ~ ->{1.0} m3OC6HSL
lasI ~ ->{1.0} m3OC12HSL
ccdA ~ ccdB ->{1.0}
c[m3OC6HSL] ->{0.5} m3OC6HSL
m3OC6HSL ->{0.5} c[m3OC6HSL]
c[m3OC12HSL] ->{0.5} m3OC12HSL
m3OC12HSL ->{0.5} c[m3OC12HSL]
luxR::m3OC6HSL ->{1.0} luxR + m3OC6HSL
cviR::m3OC6HSL ->{1.0} cviR + m3OC6HSL
cviR::m3OC12HSL ->{1.0} cviR + m3OC12HSL
lasR::m3OC12HSL ->{1.0} lasR + m3OC12HSL
ccdA2 ~ ccdB ->{0.00001}
lacI + iptg ->{1.0} lacI::iptg
tetR + aTc ->{1.0} tetR::aTc"