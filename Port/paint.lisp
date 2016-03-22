(cl:in-package #:clim3-port)

(defgeneric clim3-ext:paint-pixel (port r g b alpha))

(defgeneric clim3-ext:paint-mask (port mask color))

(defgeneric clim3-ext:paint-opaque (port color))

(defgeneric clim3-ext:paint-translucent (port color opacity))

(defgeneric clim3-ext:paint-text (port text text-style color))

(defgeneric clim3-ext:paint-trapezoids (port trapezoids color))

(defgeneric clim3-ext:paint-paths (port paths color stroke-width))

(defun clim3:paint-pixel (r g b alpha)
  (clim3-ext:paint-pixel clim3:*port* r g b alpha))

(defun clim3:paint-mask (mask color)
  (clim3-ext:paint-mask clim3:*port* mask color))

(defun clim3:paint-opaque (color)
  (clim3-ext:paint-opaque clim3:*port* color))

(defun clim3:paint-translucent (color opacity)
  (clim3-ext:paint-translucent clim3:*port* color opacity))

(defun clim3:paint-text (text text-style color)
  (clim3-ext:paint-text clim3:*port* text text-style color))

(defun clim3:paint-trapezoids (trapezoids color)
  (clim3-ext:paint-trapezoids clim3:*port* trapezoids color))

(defun clim3:paint-polygons (polygons color)
  (clim3-ext:paint-trapezoids clim3:*port* (clim3-rendering:trapezoids-from-polygons polygons) color))

(defun clim3:paint-paths (paths color stroke-width)
  (clim3-ext:paint-paths clim3:*port* paths color stroke-width))
