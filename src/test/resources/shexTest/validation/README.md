
pattern  | ShEx file
--- | ---
/bc/     | 1literalPattern            
/^bc/    | 1literalStartPattern       
/bc$/    | 1literalPatternEnd         
/\^bc\$/ | 1literalCarrotPatternDollar

literal | data file
--- | ---
ab   | Is1_Ip1_Lab.ttl
bc   | Is1_Ip1_Lbc.ttl
cd   | Is1_Ip1_Lcd.ttl
^bc$ | Is1_Ip1_LCarrotbcDollar.ttl


<table>
<tr><th></th><th>ab</th><th> bc</th><th>cd</th><th>^bc$</th></tr>
<tr><td>/bc/    </td><td> f </td><td> p </td><td>f </td><td>p    </td></tr>
<tr><td></td><td colspan="4">&lt;#1literalPattern_fail-ab&gt;</td></tr>
<tr><td></td><td colspan="4">&lt;#1literalPattern_pass-lit-match&gt;</td></tr>
<tr><td></td><td colspan="4">&lt;#1literalPattern_fail-cd&gt;</td></tr>
<tr><td></td><td colspan="4">&lt;#1literalPattern_pass-CarrotbcDollar&gt;</td></tr>
<tr><td>/^bc/   </td><td>   </td><td> p </td><td>  </td><td>f  </td></tr>
<tr><td></td><td colspan="4">&lt;#1literalStartPattern_pass-bc&gt;</td></tr>
<tr><td></td><td colspan="4">&lt;#1literalStartPattern_fail-CarrotbcDollar&gt;</td></tr>
<tr><td>/bc$/   </td><td>   </td><td> p </td><td>  </td><td>f  </td></tr>                
<tr><td></td><td colspan="4">&lt;#1literalPatternEnd_pass-bc&gt;</td></tr>
<tr><td></td><td colspan="4">&lt;#1literalPatternEnd_fail-CarrotbcDollar&gt;</td></tr>
<tr><td>/\^bc\$/</td><td>   </td><td> f </td><td>  </td><td>p  </td></tr>
<tr><td></td><td colspan="4">&lt;#1literalCarrotPatternDollar_fail-bc&gt;</td></tr>
<tr><td></td><td colspan="4">&lt;#1literalCarrotPatternDollar_pass-CarrotbcDollar&gt;</td></tr>
</table>
