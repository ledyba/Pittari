<apply template="base"> <bind tag="page-title">ぴったり印刷くん</bind>
<p>画像を指定したサイズにぴったり印刷しよう！コンビニのプリンタにも対応！</p>
<p>画像をセンチメートル単位で指定したサイズに印刷するためのpdfファイルを生成するサービスです。</p>
<p>フォトレタッチソフトだと、自宅のプリンタならともかく、コンビニや学校の共用プリンタでうまくサイズを指定して印刷できません。このサービスで生成したpdfを等倍印刷すれば、指定したサイズの大きさに画像を印刷できます。</p>
<form method='post' action='./upload' enctype="multipart/form-data">
	<table id='info'>
		<tr>
			<td>画像（10MBまで）</td>
			<td><input type='file' name='image' size='20' /></td>
		</tr>
		<tr>
			<td>画像を印刷するサイズ</td>
			<td>横<input type="number" name='imageWidth' size='5' />cm x 縦<input type="number" name='imageHeight' size='5' />cm</td>
		</tr>
		<tr>
			<td>用紙サイズ</td>
			<td>
				<select name="paper">
				<option value="A1">A1</option>
				<option value="A2">A2</option>
				<option value="A3">A3</option>
				<option value="A4" selected="selected">A4</option>
				<option value="A5">A5</option>
				<option value="A6">A6</option>
				<option value="B1">B1</option>
				<option value="B2">B2</option>
				<option value="B3">B3</option>
				<option value="B4">B4</option>
				<option value="B5">B5</option>
				<option value="B6">B6</option>
				</select>
			</td>
		</tr>
		<tr>
			<td></td>
			<td><input type='submit' value='send' /></td>
		</tr>
	</table>
</form>
<h2>セブンイレブンで印刷するには</h2>
<p>この生成されたPDFは<a href="http://www.printing.ne.jp/">セブンイレブンのネットプリント</a>に対応しています（2014年2月現在）。他のコンビニでも対応しているかもしれませんが、わかりません。</p>
<p>生成されたPDFを登録してお店で印刷すると、指定したサイズで画像を印刷できます。「原寸出力」を必ず選んで下さい。</p>
<h2>免責</h2>
このサービスは無保証です。このサービスを使って生じた不利益に対して責任を負いません。
<h2>連絡先</h2>
<a href="http://www.google.com/recaptcha/mailhide/d?k=01E-Ku7E6ON187lp0x_zBAjA==&amp;c=Cqpn7LRsAVxt-H3KuS3nEw==" onclick="window.open('http://www.google.com/recaptcha/mailhide/d?k\07501E-Ku7E6ON187lp0x_zBAjA\75\75\46c\75Cqpn7LRsAVxt-H3KuS3nEw\75\075', '', 'toolbar=0,scrollbars=0,location=0,statusbar=0,menubar=0,resizable=0,width=500,height=300'); return false;" title="Reveal this e-mail address"><img alt="frfoprkpogrg.gif" src="http://ledyba.org/wp/wp-content/uploads/files/frfoprkpogrg.gif" width="145" height="25" class="mt-image-center" style="text-align: center; display: block; margin: 0 auto 0px;" /></a>
<h2>Powerd by</h2>
このアプリは<a href="http://snapframework.com/">Snap: A Haskell Web Framework</a>で構築されています。

</apply>
