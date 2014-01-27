<apply template="base">
  <bind tag="page-title">リサイズくん</bind>
      <form method='post' action='/upload' enctype="multipart/form-data">
      <table id='info'>
        <tr>
          <td>file: </td><td><input type='file' name='image' size='20' /></td>
        </tr>
        <tr>
          <td></td>
          <td><input type='submit' value='send' /></td>
        </tr>
      </table>
    </form>
</apply>
