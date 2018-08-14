<?php

class GDException extends Exception{
  public function __construct($message = null, $code = 0, Exception $previous = null){
    parent::__construct($message, $code, $previous);
  }
}

class GDImg{
  protected $res;
  protected $imagetype;
  protected $width;
  protected $height;
  
  protected function call($errmsg, $func = null, array $arg = null){
    $result = @call_user_func_array($func, $arg);
    if(!$result){
      throw new GDException($errmsg);
    }else{
      return $result;
    }
  }

  protected function setsize($width, $height){
    $this->width = $width;
    $this->height = $height;
  }
  
  public function __construct($filename){
    $mycall = function ($func, array $arg) { return $this->call('Error loading', $func, $arg); };
    list($width, $height, $imagetype) = $mycall('getimagesize', array ($filename));
    switch($imagetype){
      case IMAGETYPE_JPEG:
        $this->res = $mycall('imagecreatefromjpeg', array ($filename)); break;
      case IMAGETYPE_PNG:
        $this->res = $mycall('imagecreatefrompng', array ($filename)); break;
      default:
        $this->call('Unsupported image type'); break;
    }
    $this->setsize($width, $height);
    $this->imagetype = $imagetype;
  }
  
  public function __destruct(){
    $this->call('Error destroying', 'imagedestroy', array ($this->res));
  }
  
  public function __clone(){
    $mycall = function ($func, array $arg) { return $this->call('Error cloning', $func, $arg); };
    $img = $mycall('imagecreatetruecolor', array ($this->width, $this->height));
    $mycall('imagealphablending', array ($img, false));
    $mycall('imagesavealpha', array ($img, true));
    $mycall('imagecopy', array ($img, $this->res, 0, 0, 0, 0, $this->width, $this->height));
    $this->res = $img;
  }
  
  public function getwidth(){
    return $this->width;
  }
  
  public function getheight(){
    return $this->height;
  }
  
  public function changetype($imagetype){
    switch($imagetype){
      case IMAGETYPE_PNG:
      case IMAGETYPE_JPEG:
        $this->imagetype = $imagetype; break;
      default:
        $this->call('changetype: Unsupported image type'); break;
    }
  }
  
  public function resize($width, $height){
    $mycall = function ($func, array $arg) { return $this->call('Error resizing', $func, $arg); };
    $img = $mycall('imagecreatetruecolor', array ($width, $height));
    $mycall('imagealphablending', array ($img, false));
    $mycall('imagesavealpha', array ($img, true));
    $mycall('imagecopyresampled', array ($img, $this->res, 0, 0, 0, 0, $width, $height, $this->width, $this->height));
    $mycall('imagedestroy', array ($this->res));
    $this->res = $img;
    $this->setsize($width, $height);
  }
  
  public function spew($filename){
    $mycall = function ($func, array $arg) { return $this->call('Error spewing', $func, $arg); };
    switch($this->imagetype){
      case IMAGETYPE_JPEG:
        $mycall('imagejpeg', array ($this->res, $filename)); break;
      case IMAGETYPE_PNG:
        $mycall('imagepng', array ($this->res, $filename)); break;
      default:
        $this->call('spew: Unsupported image type'); break;
    }
  }
}