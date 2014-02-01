<apply template="base"> <bind tag="page-title">ぴったり印刷くん</bind>
画像を指定したサイズにぴったり印刷しよう！コンビニのプリンタにも対応！
<form method='post' action='/upload' enctype="multipart/form-data">
	<table id='info'>
		<tr>
			<td>画像（10MBまで）</td>
			<td><input type='file' name='image' size='20' /></td>
		</tr>
		<tr>
			<td>印刷したいサイズ</td>
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
</apply>
